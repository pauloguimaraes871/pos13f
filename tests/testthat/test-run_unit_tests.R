# A throwaway test folder, so the package's own suite is not run recursively
make_test_dir <- function(test_code, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  writeLines(test_code, file.path(dir, "test-example.R"))
  dir
}

test_that("run_unit_tests passes and logs when every unit test passes", {
  dir <- make_test_dir('test_that("ok", expect_equal(1 + 1, 2))')
  log_path <- withr::local_tempdir()

  expect_invisible(run_unit_tests(dir, load_package = "none", log_path = log_path))
  expect_length(list.files(log_path, pattern = "^unit_tests_"), 1)
})

test_that("run_unit_tests stops the pipeline when a unit test fails", {
  dir <- make_test_dir('test_that("broken", expect_equal(1 + 1, 3))')

  expect_error(
    suppressMessages(run_unit_tests(dir,
      load_package = "none",
      log_path = withr::local_tempdir()
    )),
    "Tests failed"
  )
})

test_that("run_unit_tests rejects a missing test folder", {
  expect_error(run_unit_tests(file.path(tempdir(), "no_such_dir")), "test_path")
})
