test_that("rank autocorrelation compares consecutive formations over common stocks", {
  signals <- tibble::tibble(
    tickers = c("A", "B", "C", "D", "A", "B", "C", "E"),
    dates = as.Date(rep(c("2019-09-01", "2019-12-01"), each = 4)),
    signal_age_months = 0,
    f1_breadth_change = c(1, 2, 3, 4, 3, 2, 1, 9)
  )
  out <- report_signal_persistence(signals)
  expect_equal(out$formation, as.Date("2019-12-01"))
  expect_equal(out$n, 3)
  expect_equal(out$rank_autocorrelation, -1)
})
