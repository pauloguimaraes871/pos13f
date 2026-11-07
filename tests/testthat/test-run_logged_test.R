test_that("run_logged_test returns TRUE and writes a log when all checks pass", {
  log_path <- withr::local_tempdir()

  ok <- run_logged_test(
    function() testthat::test_that("rows are positive", testthat::expect_true(TRUE)),
    log_file = "test_bronze", log_path = log_path
  )

  expect_true(ok)
  logs <- list.files(log_path, pattern = "^test_bronze_.*\\.log$", full.names = TRUE)
  expect_length(logs, 1)
  expect_equal(attr(ok, "log_file"), logs)
})

test_that("run_logged_test returns FALSE and logs the failure when a check fails", {
  log_path <- withr::local_tempdir()

  ok <- suppressMessages(run_logged_test(
    function() testthat::test_that("row counts match", testthat::expect_equal(1, 2)),
    log_file = "test_bronze", log_path = log_path
  ))

  expect_false(ok)
  log_text <- readLines(attr(ok, "log_file"))
  expect_true(any(grepl("row counts match", log_text)))
})

test_that("run_logged_test restores console output after running", {
  sinks_before <- sink.number()
  suppressMessages(run_logged_test(function() stop("boom"), "test_x", withr::local_tempdir()))
  expect_equal(sink.number(), sinks_before)
})

test_that("run_logged_test rejects invalid arguments", {
  expect_error(run_logged_test("not a function", "x", tempdir()), "fun")
  expect_error(run_logged_test(function() NULL, "", tempdir()), "log_file")
})
