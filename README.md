maestro
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

`maestro` is a lightweight and easy-to-use framework for creating and
orchestrating data pipelines in R. No additional orchestration tools are
needed.

In `maestro` there are pipelines (functions) that can be scheduled and
configured using `roxygen2` tags - these are special comment above each
function. There is also an orchestrator script responsible for executing
the scheduled pipelines (optionally in parallel).

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
#' @maestroFrequency day
#' @maestroInterval 1
#' @maestroStartTime 2024-03-25 12:30:00
my_etl <- function() {
  
  # Extract data from random user generator
  message("API request")
  raw_data <- httr2::request("https://randomuser.me/api/") |> 
    httr2::req_perform() |> 
    httr2::resp_body_json(simplifyVector = TRUE)
  
  # Transform - get results and clean the names
  message("Transforming")
  transformed <- raw_data$results |> 
    janitor::clean_names()
  
  # Load - write to a location
  message("Writing")
  write.csv(transformed, file = paste0("random_user_", Sys.Date(), ".csv"))
}
```

What makes this a `maestro` pipeline is the use of special
*roxygen*-style comments above the function definition.
`#' @maestroFrequency day` indicates that this function should execute
at a daily frequency, `#' @maestroInterval 1` tells us it should be
every day, and `#' @maestroStartTime 2024-03-25 12:30:00` denotes the
first time it should run. In other words, we’d expect it to run every
day at 12:30 starting the 25th of March 2024. But this pipeline won’t
run at all unless there is another process *telling* it to run. That is
the job of the orchestrator.

### Orchestrator

The orchestrator is a script that checks the schedules of all the
functions in a `maestro` project and executes them if they’re due to go.
The orchestrator also handles global execution tasks such as collecting
logs and managing shared resources like database connections, global
objects, and custom functions.

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
  orch_frequency = "day",
  orch_interval = 1
)

run_status
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/unnamed-chunk-3-dark.svg">
<img src="man/figures/README-/unnamed-chunk-3.svg" width="100%" />
</picture>

The function `build_schedule()` scours through all the pipelines in the
provided directory and builds a schedule. Then `run_schedule()` checks
each pipeline’s scheduled time against the system time within some
margin of rounding and calls those pipelines to run. The output of
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

### How Scheduling Works

Both the pipelines and the orchestrator itself need to be explicitly
scheduled. The pipelines are scheduled using tags, but the orchestrator
is scheduled using arguments passed to `run_schedule()`. When
`run_schedule()` executes, it compares the next expected run time of
each pipeline and compares it with the current time. Depending on the
frequency of the orchestrator, it will round within some degree of time
difference.

For example, let’s say we have a pipeline scheduled to run hourly at
10:02am and our orchestrator runs every hour on the 00 minute. When the
orchestrator runs, it’ll be slightly before the pipeline scheduled time,
but it’ll run it anyway because it’s within a difference of an hour. If
instead our orchestrator ran every 15 minutes, it’d only execute the
pipeline once in the hour, as expected.

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
