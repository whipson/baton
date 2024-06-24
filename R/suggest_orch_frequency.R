#' Suggest an orchestrator frequency based on a schedule
#'
#' Suggests a frequency to run the orchestrator based on the frequencies of the
#' pipelines in a schedule.
#'
#' This function uses a simple heuristic to suggest an orchestrator frequency. It
#' halves the frequency of the most frequent pipeline in the schedule, unless that
#' frequency is less than or equal 15 minutes, in which case it is just the highest
#' frequency.
#'
#' @param schedule schedule data.frame created by `build_schedule()`
#'
#' @return frequency string
#' @export
#'
#' @examples
#' pipeline_dir <- tempdir()
#' create_pipeline("my_new_pipeline", pipeline_dir, open = FALSE, overwrite = TRUE)
#' schedule <- build_schedule(pipeline_dir = pipeline_dir)
#' suggest_orch_frequency(schedule)
suggest_orch_frequency <- function(schedule) {

  # Check that schedule is a data.frame
  if (!"data.frame" %in% class(schedule)) {
    cli::cli_abort(
      c("Schedule must be a data.frame and not an object of class {class(schedule)}.",
        "i" = "Use {.fn build_schedule} to create a valid schedule."),
      call = rlang::caller_env()
    )
  }

  # Check that schedule has at least one row
  if (nrow(schedule) == 0) {
    cli::cli_abort(
      c("Empty schedule. Schedule must have at least one row.",
        "i" = "Use {.fn build_schedule} to create a valid schedule."),
      call = rlang::caller_env()
    )
  }

  if (!"frequency" %in% names(schedule)) {
    cli::cli_abort(
      c("Schedule is missing required column 'frequency'.",
        "i" = "Use {.fn build_schedule} to create a valid schedule."
      ),
      call = rlang::caller_env()
    )
  }

  if (typeof(schedule$frequency) != "character") {
    cli::cli_abort(
      c("Schedule columns {.code frequency} must have type 'character'.",
        "i" = "Use {.fn build_schedule} to create a valid schedule."),
      call = rlang::caller_env()
    )
  }

  sch_secs <- purrr::map_int(
    schedule$frequency,
    purrr::possibly(convert_to_seconds, otherwise = NA, quiet = TRUE)
  )

  if (all(is.na(sch_secs))) {
    cli::cli_abort(
      c("All time units were invalid.",
        "i" = "Use {.fn build_schedule} to create a valid schedule.")
    )
  }

  if (any(is.na(sch_secs))) {
    cli::cli_warn(
      c("Some time units were invalid.",
        "i" = "Use {.fn build_schedule} to create a valid schedule.")
    )
  }

  min_freq <- schedule$frequency[which.min(sch_secs)][[1]]

  if (min(sch_secs, na.rm = TRUE) <= 60 * 15) {
    return(min_freq)
  }

  nunits <- parse_rounding_unit(min_freq)

  if (nunits$n == 1) {

    frequency <- dplyr::case_when(
      nunits$unit == "year" ~ "6 months",
      nunits$unit == "quarter" ~ "2 months",
      nunits$unit == "month" ~ "2 weeks",
      nunits$unit == "week" ~ "4 days",
      nunits$unit == "day" ~ "12 hours",
      nunits$unit == "hour" ~ "30 minutes"
    )
  } else {
    new_n <- ceiling(nunits$n / 2)
    frequency <- paste(new_n, nunits$unit)
  }

  return(frequency)
}
