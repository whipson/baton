% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_maestro.R
\name{create_maestro}
\alias{create_maestro}
\title{Creates a new maestro project}
\usage{
create_maestro(path = ".", type = "R", ...)
}
\arguments{
\item{path}{file path for the orchestrator script}

\item{type}{file type for the orchestrator (supports R, Quarto, and RMarkdown)}

\item{...}{unused}
}
\value{
invisible
}
\description{
Creates a new maestro project
}
\examples{

# Creates a new maestro project with an R orchestrator
new_proj_dir <- tempdir()
create_maestro(new_proj_dir, type = "R")

create_maestro(new_proj_dir, type = "Quarto")
}
