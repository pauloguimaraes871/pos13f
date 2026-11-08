ff_fixture <- function() {
  dates <- seq(as.Date("2014-01-01"), as.Date("2026-07-01"), by = "month")
  tibble::tibble(
    dates = dates, mkt_rf = 1, smb = 0.2, hml = -0.3, rmw = 0.1, cma = 0, rf = 0.2, umd = 0.5
  )
}
ff_suite_passes <- function(factors, env = parent.frame()) {
  suppressMessages(run_logged_test(function() run_test_bronze_ff_factors(factors),
    log_file = "test_ff", log_path = withr::local_tempdir(.local_envir = env)
  ))
}

test_that("a complete monthly series in percentage points passes", {
  expect_true(ff_suite_passes(ff_fixture()))
})

test_that("a gap, a short window or decimal units fail", {
  expect_false(ff_suite_passes(ff_fixture()[-10, ]))
  expect_false(ff_suite_passes(dplyr::filter(ff_fixture(), .data$dates <= as.Date("2025-01-01"))))
  expect_false(ff_suite_passes(dplyr::mutate(ff_fixture(), rf = 3, mkt_rf = 50)))
})
