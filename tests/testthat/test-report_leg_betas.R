## Two quantiles built with known betas (0.5 and 1.5) over 12 months; the factors stop
## one month early
leg_fixture <- function() {
  months <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  mkt <- c(1, -2, 3, 0.5, -1, 2, -0.5, 1.5, -3, 2.5, 0, 1)
  rf <- 0.1
  list(
    ladder = tibble::tibble(
      signal = "f1", method = "cs", design = "unconditional",
      quantile = rep(1:2, each = 12), dates = rep(months, 2),
      raw_return = c(rf + 0.2 + 0.5 * mkt, rf - 0.1 + 1.5 * mkt)
    ),
    factors = tibble::tibble(dates = months[-12], mkt_rf = mkt[-12], rf = rf)
  )
}

test_that("each quantile's beta is the slope of its excess return on the market", {
  x <- leg_fixture()
  out <- report_leg_betas(x$ladder, x$factors)
  expect_equal(out$beta, c(0.5, 1.5))
  expect_equal(out$n_months, c(11, 11))
})

test_that("missing columns are refused", {
  x <- leg_fixture()
  expect_error(report_leg_betas(x$ladder[, -6], x$factors), "raw_return")
  expect_error(report_leg_betas(x$ladder, x$factors[, 1:2]), "rf")
})
