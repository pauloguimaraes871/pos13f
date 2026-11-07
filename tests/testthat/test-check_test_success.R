test_that("check_test_success passes silently on TRUE", {
  expect_invisible(check_test_success(TRUE))
})

test_that("check_test_success stops the deployment and names the log on failure", {
  failed <- structure(FALSE, log_file = "logs/tests/test_bronze_2026.log")
  expect_error(check_test_success(failed), "test_bronze_2026.log")
  expect_error(check_test_success(NA), "Tests failed")
})
