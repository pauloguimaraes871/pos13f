test_that("UMD is summarised over the window and its full history", {
  set.seed(3)
  factors <- tibble::tibble(
    dates = seq(as.Date("2010-01-01"), by = "month", length.out = 200),
    mkt_rf = stats::rnorm(200, 0.5, 4), umd = stats::rnorm(200, 0.6, 4)
  )
  out <- report_momentum_context(factors, first_month = as.Date("2014-03-01"))
  expect_equal(out$umd$period, c("study window", "since 1963"))
  expect_equal(out$umd$n_months[2], 200)
  expect_equal(out$umd$ann_mean[2], 12 * mean(factors$umd))
  expect_true(all(out$umd_by_year$year >= 2014))
})
