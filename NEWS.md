# maestro 0.1.1

## Breaking changes

- Creater functions `create_pipeline()` and `create_maestro` no longer have default arguments for the path to where the scripts are 
created. Users must explicitly define these paths.

- Argument `log_file` in `run_schedule()` no longer defaults to `./maestro.log` but instead defaults to `NULL`.

## Minor changes

- Creater functions `create_*` now take a boolean `overwrite` argument to make the overwriting of existing pipelines, projects, and orchestrators more explicit.

# maestro 0.1.0

- Initial CRAN submission

# maestro 0.0.4

## Bug fixes

- Fixed the output of the next run for pipelines (#90)

# maestro 0.0.3

## Breaking changes

- `run_schedule()` now returns a list with status and artifacts instead of just a data.frame of the status. Artifacts are any values returned from pipelines. Pipelines that return nothing will have no artifacts.

## Major changes

- New helper function `suggest_orch_frequency()` to provide a suggestion of what frequency to use for the orchestrator.

## Minor changes

- Start and end times are now reported from functions that result in an error in single core only (#82).

## Bug fixes

- CLI output from `run_schedule()` now correctly outputs the total number of pipelines (#81) and correctly outputs number of errors.

# maestro 0.0.2

## Breaking changes

- maestroFrequency tag now adheres to a more human-readable format like "1 day", "2 hours", "4 weeks", etc.

- `orch_frequency` argument in `run_schedule()` also takes more human-readable format identical to maestroFrequency tag.

- maestroInterval tag removed

- `orch_interval` argument to `run_schedule()` removed.

- `create_maestro()` and `create_orchestrator()` now use the argument `type` instead of `extension` for defining what script type to use for the orchestrator.

- Changed `last_parsing_errors()` to `last_build_errors()`; changed functions of the form `last_runtime_*()` to `last_run_*()`.

## Major changes

- Additional columns added to the output of `run_schedule()`: `pipeline_started` and `pipeline_ended` to indicate the start and end times of a pipeline execution; `next_run` to indicate when the next run should be based on the frequency of the pipeline and orchestrator.

- Pipelines now show as skipped if they are not scheduled.

- Added hex logo

## Minor changes

- Backend improvements to schedule checking

- Timestamps are formatted to specified time zone.

- `run_schedule()` cli output suggests to use `last_run_errors()` or `last_run_warnings()` if any errors or warnings were found.
