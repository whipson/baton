maestro
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/whipson/maestro/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/whipson/maestro/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`maestro` is a lightweight framework for creating and orchestrating data
pipelines in R. If you have several scheduled jobs or pipelines you want
to run in R and want to manage them in one project, then `maestro` is
for you!

In `maestro` you create **pipelines** (functions) and schedule them
using `roxygen2` tags - these are special comments (decorators) above
each function. Then you create an **orchestrator** containing `maestro`
functions for scheduling and invoking the pipelines.

## Pre-release Disclaimer

`maestro` is in early development and its API may undergo changes
without notice or deprecation. We encourage people to try it out in real
world scenarios, but we do not yet advise using it in critical
production environments until it has been thoroughly tested and the API
has stabilized.

## Installation

`maestro` is currently pre-release and not available yet on CRAN. It can
be installed from Github directly like so:

``` r
devtools::install_github("https://github.com/whipson/maestro")
```

## Project Setup

A `maestro` project needs at least two components:

1.  A collection of R pipelines (functions) that you want to schedule
2.  A single orchestrator script that kicks off the scripts when they’re
    scheduled to run

The project file structure will look like this:

    sample_project
    ├── orchestrator.R
    └── pipelines
        ├── my_etl.R
        ├── pipe1.R
        └── pipe2.R

Let’s look at each of these in more detail.

### Pipelines

A pipeline is task we want to run. This task may involve retrieving data
from a source, performing cleaning and computation on the data, then
sending it to a destination. `maestro` is not concerned with what your
pipeline does, but rather *when* you want to run it. Here’s a simple
pipeline in `maestro`:

``` r
#' Example ETL pipeline
#' @maestroFrequency 1 day
#' @maestroStartTime 2024-03-25 12:30:00
my_etl <- function() {
  
  # Pretend we're getting data from a source
  message("Get data")
  extracted <- mtcars
  
  # Transform
  message("Transforming")
  transformed <- extracted |> 
    dplyr::mutate(hp_deviation = hp - mean(hp))
  
  # Load - write to a location
  message("Writing")
  write.csv(transformed, file = paste0("transformed_mtcars_", Sys.Date(), ".csv"))
}
```

What makes this a `maestro` pipeline is the use of special
*roxygen*-style comments above the function definition:

- `#' @maestroFrequency 1 day` indicates that this function should
  execute at a daily frequency.

- `#' @maestroStartTime 2024-03-25 12:30:00` denotes the first time it
  should run.

In other words, we’d expect it to run every day at 12:30 starting the
25th of March 2024. There are more `maestro` tags than these ones and
all follow the camelCase convention established by `roxygen2`.

### Orchestrator

The orchestrator is a script that checks the schedules of all the
pipelines in a `maestro` project and executes them. The orchestrator
also handles global execution tasks such as collecting logs and managing
shared resources like global objects and custom functions.

You have the option of using Quarto, RMarkdown, or a straight-up R
script for the orchestrator, but the former two have some advantages
with respect to deployment on Posit Connect.

A simple orchestrator looks like this:

``` r
library(maestro)

# Look through the pipelines directory for maestro pipelines to create a schedule
schedule_table <- build_schedule(pipeline_dir = "pipelines")

# Checks which pipelines are due to run and then executes them
run_status <- run_schedule(
  schedule_table, 
  orch_frequency = "1 day"
)

run_status
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/unnamed-chunk-3-dark.svg">
<img src="man/figures/README-/unnamed-chunk-3.svg" width="100%" />
</picture>

The function `build_schedule()` scours through all the pipelines in the
project and builds a schedule. Then `run_schedule()` checks each
pipeline’s scheduled time against the system time within some margin of
rounding and calls those pipelines to run. The output of
`run_schedule()` itself is a table of pipeline statuses.

### Logging

`maestro` can keep an accumulating log of all messages, warnings, and
errors reported from the pipelines. This log file is created in the
project directory (by default `./maestro.log`) and accumulates until the
`log_file_max_bytes` argument is exceeded.

``` r
readLines("maestro.log") |> 
  tail(10) |> 
  cat(sep = "\n")
#> [my_etl] [INFO] [2024-05-23 14:26:46.533403]: API request
#> [my_etl] [INFO] [2024-05-23 14:26:46.805017]: Transforming
#> [my_etl] [INFO] [2024-05-23 14:26:46.846578]: Writing
#> [get_mtcars] [WARN] [2024-05-23 14:26:46.894069]: Uh oh
```

### Multicore

If you have several pipelines and/or pipelines that take awhile to run,
it can be more efficient to split computation across multiple CPU cores.

``` r
library(furrr)

plan(multisession)

run_schedule(
  schedule_table,
  cores = 4
)
```
