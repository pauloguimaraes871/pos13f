## 120 months of factors and a series built as alpha + loadings x factors + noise
alpha_fixture <- function(alpha = 0.5) {
  set.seed(1)
  n <- 120
  factors <- tibble::tibble(
    dates = seq(as.Date("2010-01-01"), by = "month", length.out = n),
    mkt_rf = stats::rnorm(n, 0.5, 4), smb = stats::rnorm(n, 0, 2), hml = stats::rnorm(n, 0, 2),
    rmw = stats::rnorm(n, 0, 1.5), cma = stats::rnorm(n, 0, 1.5), rf = 0.1,
    umd = stats::rnorm(n, 0.3, 3)
  )
  ret <- alpha + 1.1 * factors$mkt_rf - 0.4 * factors$umd + stats::rnorm(n, 0, 0.3)
  list(factors = factors, returns = tibble::tibble(series = "s", dates = factors$dates, ret = ret))
}

test_that("alpha and its Newey-West interval recover a known series", {
  x <- alpha_fixture()
  out <- summarise_backtest_returns(x$returns, x$factors)
  expect_equal(out$lag, c(3, 6))
  expect_equal(out$alpha_m[1], 0.5, tolerance = 0.05)
  expect_true(all(out$alpha_lo < 0.5 & out$alpha_hi > 0.5))
  expect_equal(out$ann_alpha, 12 * out$alpha_m)
})

test_that("long-only series are regressed in excess of the risk-free rate", {
  x <- alpha_fixture()
  raw <- summarise_backtest_returns(x$returns, x$factors, lags = 3)
  excess <- summarise_backtest_returns(x$returns, x$factors, excess_series = "s", lags = 3)
  expect_equal(raw$alpha_m - excess$alpha_m, 0.1, tolerance = 1e-8)
  bad <- dplyr::mutate(x$returns, ret = dplyr::if_else(dplyr::row_number() == 1, NA, .data$ret))
  expect_error(summarise_backtest_returns(bad, x$factors), "must not be NA")
})
