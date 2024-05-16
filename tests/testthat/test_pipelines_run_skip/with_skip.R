#' Simple mtcars print function
#'
#' This is a function that runs every hour starting at
#' 2024-03-01 09:00:00
#'
#' @maestroFrequency day
#' @maestroInterval 1
#' @maestroStartTime 2024-03-01 09:00:00
#' @maestroTz UTC
#'
#' @export
get_mtcars <- function() {
  Sys.sleep(0.2)
  mtcars
}

#' Multiply
#'
#' @maestroFrequency month
#' @maestroInterval 3
#' @maestroSkip
#'
#' @export
wait <- function() {
  Sys.sleep(0.1)
}

#' Simple mtcars print function
#'
#' This is a function that runs every hour starting at
#' 2024-03-01 09:00:00
#'
#' @maestroFrequency day
#' @maestroInterval 1
#' @maestroStartTime 2024-03-01 09:00:00
#' @maestroTz UTC
#'
#' @export
get_mtcars2 <- function() {
  Sys.sleep(0.2)
  mtcars
}