#' Run a schedule
#'
#' Given a schedule in a `maestro` project, runs the pipelines that are scheduled to execute
#' based on the current time.
#'
#' @md
#' @details
#'
#' ## Pipeline schedule logic
#'
#' The function `run_schedule()` examines each pipeline in the schedule table and determines
#' whether it is scheduled to run at the current time using some simple time arithmetic. We assume
#' `run_schedule(schedule, check_datetime = Sys.time())`, but this need not be the case.
#'
#' ## Output
#'
#' `run_schedule()` returns a list with two elements: `status` and `artifacts`. Status is a data.frame where
#' each row is a pipeline and the columns are information about the pipeline status, execution time, etc. Artifacts
#' are any values returned from pipelines.
#'
#' ## Pipelines with arguments (resources)
#'
#' If a pipeline takes an argument that doesn't include a default value, these can be supplied
#' in the orchestrator via `run_schedule(resources = list(arg1 = val))`. The name of the argument
#' used by the pipeline must match the name of the argument in the list. Currently, each named
#' resource must refer to a single object. In other words, you can't have two pipes using
#' the same argument but requiring different values.
#'
#' ## Running in parallel
#'
#' Pipelines can be run in parallel using the `cores` argument. First, you must run `future::plan(future::multisession)`
#' in the orchestrator. Then, supply the desired number of cores to the `cores` argument. Note that
#' console output appears different in multicore mode.
#'
#' @param schedule a table of scheduled pipelines generated from `build_schedule()`
#' @inheritParams check_pipelines
#' @param orch_frequency of the orchestrator, a single string formatted like "1 day" or "2 weeks"
#' @param resources named list of shared resources made available to pipelines as needed
#' @param run_all run all pipelines regardless of the schedule (default is `FALSE`) - useful for testing.
#' Does not apply to pipes with a `maestroSkip` tag.
#' @param n_show_next show the next n scheduled pipes
#' @param cores number of cpu cores to run if running in parallel. If > 1, `furrr` is used and
#' a multisession plan must be executed in the orchestrator (see details)
#' @param logging whether or not to write the logs to a file (default = `FALSE`)
#' @param log_file path to the log file (ignored if `logging == FALSE`)
#' @param log_file_max_bytes numeric specifying the maximum number of bytes allowed in the log file before purging the log (within a margin of error)
#' @param quiet silence metrics to the console (default = `FALSE`)
#'
#' @return list with named elements `status` and `artifacts`
#' @importFrom R.utils countLines
#' @export
#' @examples
#' pipeline_dir <- tempdir()
#' create_pipeline("my_new_pipeline", pipeline_dir, open = FALSE, overwrite = TRUE)
#' schedule <- build_schedule(pipeline_dir = pipeline_dir)
#' # Runs the schedule every 1 day
#' run_schedule(
#'   schedule,
#'   orch_frequency = "1 day",
#'   quiet = TRUE
#' )
#'
#' # Runs the schedule every 15 minutes
#' run_schedule(
#'   schedule,
#'   orch_frequency = "15 minutes",
#'   quiet = TRUE
#' )
run_schedule <- function(
    schedule,
    orch_frequency = "1 day",
    check_datetime = lubridate::now(tzone = "UTC"),
    resources = list(),
    run_all = FALSE,
    n_show_next = 5,
    cores = 1,
    logging = FALSE,
    log_file = NULL,
    log_file_max_bytes = 1e6,
    quiet = FALSE
  ) {

  # Check validity of the schedule
  schedule_validity_check(schedule)

  # Get the orchestrator nunits
  orch_nunits <- tryCatch({
    parse_rounding_unit(orch_frequency)
  }, error = \(e) {
    cli::cli_abort(
      c(
        "Invalid `orch_frequency` {orch_frequency}.",
        "i" = "Must be of the format like '1 day', '2 weeks', etc."
      ),
      call = NULL
    )
  })

  # Additional parse using timechange to verify it isn't something like 500 days,
  # which isn't understood by timechange
  tryCatch({
    timechange::time_round(Sys.time(), orch_frequency)
  }, error = \(e) {
    timechange_error_fmt <- gsub('\\..*', '', e$message)
    cli::cli_abort(
      c(
        "Invalid `orch_frequency` {orch_frequency}.
        {timechange_error_fmt}."
      ),
      call = NULL
    )
  })

  # Ensure that elements in resources are named
  if (length(resources) > 0) {
    resources_length <- length(resources)
    n_named <- sum(names(resources) != "")
    if (resources_length > n_named) {
      cli::cli_abort(
        "All elements in `resources` must be named."
      )
    }

    n_uniq_names <- length(unique(names(resources)))
    if (resources_length > n_uniq_names) {
      cli::cli_abort(
        "All elements in `resources` must have unique names."
      )
    }
  }

  # Parallelization
  mapper_fun <- function(...) {
    purrr::pmap(...)
  }
  if (!is.null(cores)) {
    if (cores < 1 || (cores %% 1) != 0) cli::cli_abort("`cores` must be a positive integer")
    if (cores > 1) {
      tryCatch({
        rlang::check_installed("furrr")
        mapper_fun <- function(...) {
          furrr::future_pmap(..., .options = furrr::furrr_options(stdout = FALSE))
        }
      }, error = \(e) {
        cli::cli_warn("{.pkg furrr} is required for running on multiple cores.")
      })
    }
  }

  run_schedule_fun <- function() {

    # Add a pipeline id
    schedule <- schedule |>
      dplyr::mutate(pipe_id = 1:dplyr::n())

    # Select the pipelines based on the orchestrator
    if (!run_all) {
      schedule_checks <- check_pipelines(
        orch_unit = orch_nunits$unit,
        orch_n = orch_nunits$n,
        pipeline_unit = schedule$frequency_unit,
        pipeline_n = schedule$frequency_n,
        pipeline_datetime = schedule$start_time,
        check_datetime = check_datetime
      )

      schedule <- schedule |>
        dplyr::mutate(
          invoked = purrr::map_lgl(schedule_checks, ~.x$is_scheduled_now),
          next_run = purrr::map_vec(schedule_checks, ~.x$next_run)
        )
    } else {

      schedule <- schedule |>
        dplyr::mutate(
          invoked = TRUE,
          next_run = NA
        )
    }

    if (logging) {
      if (!rlang::is_scalar_character(log_file)) cli::cli_abort(
        "When {.code logging == TRUE}, {.code log_file} must be a single character."
      )
      if (!file.exists(log_file)) file.create(log_file)
    } else {
      log_file <- tempfile()
    }

    cli::cli_h3("Running pipelines {cli::col_green(cli::symbol$play)}")

    tictoc::tic(quiet = quiet)

    # Create external vars for these (this won't work in parallel)
    orchestrator_context <- new.env()
    assign("start_times", lubridate::POSIXct(tz = "UTC"), envir = orchestrator_context)
    assign("end_times", lubridate::POSIXct(tz = "UTC"), envir = orchestrator_context)

    # Execute the schedule (possibly in parallel)
    runs <- mapper_fun(
      list(
        schedule$script_path,
        schedule$pipe_name,
        schedule$skip,
        schedule$log_level,
        schedule$invoked
      ),
      purrr::safely(
        ~{
          if (..3 || !..5) {
            cli::cli_alert("{cli::col_silver(..1)} { ..2}")
            assign(
              "start_times",
              c(get("start_times", envir = orchestrator_context), NA),
              envir = orchestrator_context
            )
            assign(
              "end_times",
              c(get("end_times", envir = orchestrator_context), NA),
              envir = orchestrator_context
            )
            return(
              list(
                warnings = NULL,
                messages = NULL,
                skip = TRUE
              )
            )
          } else {
            cli::cli_progress_step("{cli::col_silver(..1)} {.pkg { ..2}}")
            assign(
              "start_times",
              c(get("start_times", envir = orchestrator_context), lubridate::now("UTC")),
              envir = orchestrator_context
            )
            tryCatch({
              run_schedule_entry(
                ..1,
                ..2,
                resources = resources,
                log_file = log_file,
                log_level = ..4,
                log_file_max_bytes = log_file_max_bytes
              )
            }, finally = {
              assign(
                "end_times",
                c(get("end_times", envir = orchestrator_context),
                  lubridate::now("UTC")),
                envir = orchestrator_context
                )
            })
          }
        },
        quiet = TRUE
      )
    ) |>
      purrr::set_names(
        schedule$pipe_id
      )

    elapsed <- tictoc::toc(quiet = TRUE)

    # Get the start/end times from the orchestrator context
    start_times <- get("start_times", envir = orchestrator_context)
    end_times <- get("end_times", envir = orchestrator_context)
    start_times <- if (length(start_times) == 0) NA else start_times
    end_times <- if (length(end_times) == 0) NA else end_times

    # Get the status as a tibble
    status <- purrr::imap(
      runs,
      ~dplyr::tibble(
        pipe_id = as.integer(.y),
        errors = as.integer(length(.x$error) > 0),
        warnings = length(.x$result$warnings),
        messages = length(.x$result$messages)
      )
    ) |>
      purrr::list_rbind() |>
      dplyr::mutate(
        pipeline_started = start_times,
        pipeline_ended = end_times
      )

    # Modify the names
    runs <- runs |>
      purrr::set_names(glue::glue("{schedule$pipe_name} {schedule$script_path}"))

    # Get the errors
    run_errors <- runs |>
      purrr::map(
        ~.x$error
      ) |>
      purrr::discard(is.null)

    # Get the warnings
    run_warnings <- runs |>
      purrr::map(
        ~.x$result$warnings
      ) |>
      purrr::discard(is.null)

    # Get the messages
    run_messages <- runs |>
      purrr::map(
        ~.x$result$messages
      ) |>
      purrr::discard(is.null)

    # For access via last_run_*
    maestro_pkgenv$last_run_errors <- run_errors
    maestro_pkgenv$last_run_warnings <- run_warnings
    maestro_pkgenv$last_run_messages <- run_messages

    # Create status table
    status_table <- schedule |>
      dplyr::left_join(status, by = "pipe_id") |>
      dplyr::mutate(
        success = ifelse(invoked, errors == 0, NA),
        dplyr::across(
          dplyr::where(lubridate::is.POSIXct),
          ~lubridate::with_tz(.x, tzone = lubridate::tz(check_datetime))
        )
      ) |>
      dplyr::select(
        pipe_name,
        script_path,
        invoked,
        success,
        pipeline_started,
        pipeline_ended,
        errors,
        warnings,
        messages,
        next_run
      )

    # Get the artifacts as a named list
    artifacts <- runs |>
      purrr::map(
        ~.x$result$artifacts
      ) |>
      purrr::discard(is.null)

    # Get the number of statuses
    total <- nrow(status_table)
    invoked <- sum(status_table$invoked)
    error_count <- length(run_errors)
    skip_count <- sum(!status_table$invoked)
    success_count <- invoked - error_count
    warning_count <- length(run_warnings)

    cli::cli_h3("Pipeline execution completed {cli::col_silver(cli::symbol$stop)} | {elapsed$callback_msg}")

    cli::cli_text(
      "
      {cli::col_green(cli::symbol$tick)} {success_count} success{?es} |
      {cli::col_black(cli::symbol$arrow_right)} {skip_count} skipped |
      {cli::col_magenta('!')} {warning_count} warning{?s} |
      {cli::col_red(cli::symbol$cross)} {error_count} error{?s} |
      {cli::col_cyan(cli::symbol$square_small_filled)} {total} total
      "
    )

    if (error_count > 0) {
      cli::cli_alert_danger(
        "Use {.fn last_run_errors} to show pipeline errors."
      )
    }

    if (warning_count > 0) {
      cli::cli_alert_warning(
        "Use {.fn last_run_warnings} to show pipeline warnings."
      )
    }

    cli::cli_rule()


    # Output for showing next pipelines schedule
    if (!run_all && n_show_next > 0 && nrow(schedule) > 0) {

      next_runs_cli <- status_table |>
        dplyr::arrange(next_run) |>
        utils::head(n = n_show_next)

      cli::cli_h3("Next scheduled pipelines {cli::col_cyan(cli::symbol$pointer)}")
      next_run_strs <- glue::glue("{next_runs_cli$pipe_name} | {next_runs_cli$next_run}")
      cli::cli_text("Pipe name | Next scheduled run")
      cli::cli_ul(next_run_strs)
    }

    output <- list(
      status = status_table,
      artifacts = artifacts
    )

    return(output)
  }

  if (quiet) {
    run_schedule_fun <- purrr::quietly(run_schedule_fun)
    output <- run_schedule_fun() |>
      purrr::pluck("result")
  } else {
    output <- run_schedule_fun()
  }

  return(output)
}
