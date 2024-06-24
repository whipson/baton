## Resubmission
This is a resubmission. In this version I have:

* Removed the redundant 'in R' in the description section of the DESCRIPTION.

* Removed the extra white space in the description section of the DESCRIPTION.

* Removed default value to `path` argument in `create_maestro()` and `create_orchestrator()`.

* Removed default value to `pipeline_dir` argument in `create_pipeline()`.

* Removed default value of working directory to `log_file` argument of `run_schedule()`. It is now `NULL`. 
Same change was made for `run_schedule_entry()`.

* Ensured that examples do not write to the current directory and instead use `tempdir()`.

* Added `overwrite` argument to `create_*` functions to make overwriting a more explicit functionality.

* There are no references describing the methods in our package so I have not added references.
