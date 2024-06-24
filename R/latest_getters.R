#' Retrieve latest maestro build errors
#'
#' Gets the latest schedule build errors following use of `build_schedule()`. If
#' the build succeeded or `build_schedule()` has not been run it will be `NULL`.
#'
#' @return error messages
#' @export
#' @examples
#' pipeline_dir <- tempdir()
#' create_pipeline("my_new_pipeline", pipeline_dir, open = FALSE, overwrite = TRUE)
#' build_schedule(pipeline_dir = pipeline_dir)
#' last_build_errors()
last_build_errors <- function() {
  maestro_pkgenv$last_build_errors
}

#' Retrieve latest maestro pipeline errors
#'
#' Gets the latest pipeline errors following use of `run_schedule()`. If
#' the all runs succeeded or `run_schedule()` has not been run it will be `NULL`.
#'
#' @return error messages
#' @export
#' @examples
#' pipeline_dir <- tempdir()
#' create_pipeline("my_new_pipeline", pipeline_dir, open = FALSE, overwrite = TRUE)
#' schedule <- build_schedule(pipeline_dir = pipeline_dir)
#' run_schedule(schedule)
#' last_run_errors()
last_run_errors <- function() {
  maestro_pkgenv$last_run_errors
}

#' Retrieve latest maestro pipeline warnings
#'
#' Gets the latest pipeline warnings following use of `run_schedule()`. If
#' there are no warnings or `run_schedule()` has not been run it will be `NULL`.
#'
#' Note that setting `maestroLogLevel` to something greater than `WARN` will
#' ignore warnings.
#'
#' @return warning messages
#' @export
#' @examples
#' pipeline_dir <- tempdir()
#' create_pipeline("my_new_pipeline", pipeline_dir, open = FALSE, overwrite = TRUE)
#' schedule <- build_schedule(pipeline_dir = pipeline_dir)
#' run_schedule(schedule)
#' last_run_warnings()
last_run_warnings <- function() {
  maestro_pkgenv$last_run_warnings
}

#' Retrieve latest maestro pipeline messages
#'
#' Gets the latest pipeline messages following use of `run_schedule()`. If
#' there are no messages or `run_schedule()` has not been run it will be `NULL`.
#'
#' Note that setting `maestroLogLevel` to something greater than `INFO` will
#' ignore messages.
#'
#' @return messages
#' @export
#' @examples
#' pipeline_dir <- tempdir()
#' create_pipeline("my_new_pipeline", pipeline_dir, open = FALSE, overwrite = TRUE)
#' schedule <- build_schedule(pipeline_dir = pipeline_dir)
#' run_schedule(schedule)
#' last_run_messages()
last_run_messages <- function() {
  maestro_pkgenv$last_run_messages
}
