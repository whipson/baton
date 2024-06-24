#' Create a new pipeline in a pipelines directory
#'
#' Allows the creation of new pipelines (R scripts) and fills in the maestro tags as specified.
#'
#' @param pipe_name name of the pipeline and function
#' @param pipeline_dir directory containing the pipeline scripts
#' @param frequency how often the pipeline should run (e.g., 1 day, 3 hours, 4 months). Fills in maestroFrequency tag
#' @param start_time start time of the pipeline schedule. Fills in maestroStartTime tag
#' @param tz timezone that pipeline will be scheduled in. Fills in maestroTz tag
#' @param log_level log level for the pipeline (e.g., INFO, WARN, ERROR). Fills in maestroLogLevel tag
#' @param open whether or not to open the script upon creation
#' @param quiet whether to silence messages in the console (default = `FALSE`)
#' @param overwrite whether or not to overwrite an existing pipeline of the same name and location.
#'
#' @return invisible
#' @export
#' @examples
#' pipeline_dir <- tempdir()
#' create_pipeline(
#'   "extract_data",
#'   pipeline_dir = pipeline_dir,
#'   frequency = "1 hour",
#'   open = FALSE,
#'   quiet = TRUE,
#'   overwrite = TRUE
#' )
#'
#' create_pipeline(
#'   "new_job",
#'   pipeline_dir = pipeline_dir,
#'   frequency = "20 minutes",
#'   start_time = as.POSIXct("2024-06-21 12:20:00"),
#'   log_level = "ERROR",
#'   open = FALSE,
#'   quiet = TRUE,
#'   overwrite = TRUE
#' )
#'
#' # Clean up
#' if (!interactive()) unlink("pipelines", recursive = TRUE)
create_pipeline <- function(
    pipe_name,
    pipeline_dir = "pipelines",
    frequency = "1 day",
    start_time = Sys.Date(),
    tz = "UTC",
    log_level = "INFO",
    quiet = FALSE,
    open = interactive(),
    overwrite = FALSE
  ) {

  # Makes a valid name for a pipe
  pipe_name <- gsub("\\.", "_", make.names(pipe_name))

  script <- readLines(system.file("pipeline_template", package = "maestro")) |>
    paste(collapse = "\n") |>
    glue::glue(
      .open = "{{",
      .close = "}}",
      .null = NULL
    )

  path <- file.path(pipeline_dir, paste0(pipe_name, ".R"))

  if (!dir.exists(pipeline_dir)) {
    dir.create(pipeline_dir)
  }

  if (file.exists(path)) {
    if (!overwrite) {
      cli::cli_abort(
        c("File {.file path} already exists.",
          "Set {.code create_pipeline(overwrite = TRUE)} to overwrite anyway."),
        call = NULL
      )
    } else {
      if (!quiet) cli::cli_alert_warning("Overwriting existing pipeline at {.file {path}}.")
    }
  }

  writeLines(
    script,
    path
  )

  if (open) {
    rstudioapi::documentOpen(path)
  }

  if(!quiet) {
    cli::cli_alert_success("Created pipeline at {.file {path}}")
  }
}
