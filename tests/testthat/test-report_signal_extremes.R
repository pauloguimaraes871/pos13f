test_that("the top and bottom F1 stocks of the last formation are listed with tickers", {
  signals <- tibble::tibble(
    tickers = rep(paste0("F", 1:4), 2),
    dates = as.Date(rep(c("2019-09-01", "2019-12-01"), each = 4)),
    signal_age_months = 0, f1_breadth_change = c(1, 2, 3, 4, 0.4, -0.2, 0.1, -0.5),
    n_holders_prev = 100, n_holders_curr = 110, cohort_size = 5000
  )
  master <- tibble::tibble(instrument_id = paste0("F", 1:4), ticker = c("AA", "BB", "CC", "DD"))
  out <- report_signal_extremes(signals, master, n = 1)
  expect_equal(out$side, c("top", "bottom"))
  expect_equal(out$ticker, c("AA", "DD"))
  expect_true(all(out$dates == as.Date("2019-12-01")))
})
