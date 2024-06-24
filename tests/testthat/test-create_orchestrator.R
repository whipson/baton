test_that("create_orchestrator creates a new orchestrator", {
  withr::with_tempdir({
    create_orchestrator("orchestrator" ,open = FALSE)
    expect_true(file.exists("orchestrator.R"))
  }) |>
    expect_message()
})
