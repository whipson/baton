#' Creates a new maestro project
#'
#' @inheritParams create_orchestrator
#' @param ... unused
#'
#' @export
#' @return invisible
#' @examples
#'
#' # Creates a new maestro project with an R orchestrator
#' new_proj_dir <- tempdir()
#' create_maestro(new_proj_dir, type = "R", overwrite = TRUE)
#'
#' create_maestro(new_proj_dir, type = "Quarto", overwrite = TRUE)
create_maestro <- function(path, type = "R", overwrite = FALSE, quiet = FALSE, ...) {

  type <- match.arg(type, choices = c("R", "Quarto", "RMarkdown"))

  path_to_maestro <- normalizePath(
    path,
    mustWork = FALSE
  )

  if (dir.exists(path_to_maestro)) {
    if (!overwrite) {
      cli::cli_abort(
        paste(
          "Project directory already exists. \n",
          "Set `create_maestro(overwrite = TRUE)` to overwrite anyway.\n",
          "This will remove any work in this directory. \n"
        ),
        call = NULL
      )
    } else {
      if (!quiet) cli::cli_alert_warning("Overwriting existing project.")
    }
  }

  if (!quiet) cli::cli_alert_success("Creating maestro project")

  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  create_orchestrator(
    path = file.path(path, "orchestrator"),
    type = type,
    open = FALSE,
    quiet = TRUE,
    overwrite = overwrite
  )

  create_pipeline(
    pipe_name = "my_pipe",
    pipeline_dir = file.path(path, "pipelines"),
    open = FALSE,
    quiet = TRUE,
    overwrite = overwrite
  )
}
