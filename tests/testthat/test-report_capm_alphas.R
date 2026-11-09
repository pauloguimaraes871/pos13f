## Two spreads over 24 months; the factor data stop one month before the returns
capm_fixture <- function() {
  months <- seq(as.Date("2018-01-01"), by = "month", length.out = 24)
  mkt <- rep(c(1.5, -2, 0.5, 3, -1, 0.8), 4)
  list(
    spreads = tibble::tibble(
      signal = "f1", method = "cs", design = "unconditional",
      cost_bps = rep(c(0, 25), each = 24), borrow_bps = rep(c(0, 100), each = 24),
      dates = rep(months, 2),
      net_return = c(
        0.4 + 0.2 * mkt + rep(c(0.3, -0.3), 12), 0.1 + 0.2 * mkt + rep(c(0.3, -0.3), 12)
      )
    ),
    factors = tibble::tibble(dates = months[-24], mkt_rf = mkt[-24])
  )
}

test_that("each spread gets the intercept and slope of its market regression", {
  x <- capm_fixture()
  out <- report_capm_alphas(x$spreads, x$factors)
  expect_equal(out$series, c("f1|cs|unconditional|spread_0_0", "f1|cs|unconditional|spread_25_100"))
  expect_equal(out$n_capm_months, c(23, 23))
  gross <- x$spreads[1:23, ]
  fit <- stats::lm(gross$net_return ~ x$factors$mkt_rf)
  expect_equal(out$capm_alpha[1], 12 * unname(stats::coef(fit)[1]))
  expect_equal(out$capm_beta[1], unname(stats::coef(fit)[2]))
  expect_true(out$capm_lo[1] < out$capm_alpha[1] && out$capm_alpha[1] < out$capm_hi[1])
})

test_that("the Newey-West error does not depend on the order of the rows", {
  x <- capm_fixture()
  shuffled <- x$spreads[withr::with_seed(1, sample(nrow(x$spreads))), ]
  ordered <- report_capm_alphas(x$spreads, x$factors)
  out <- report_capm_alphas(shuffled, x$factors)
  expect_equal(out$capm_t[match(ordered$series, out$series)], ordered$capm_t)
})

test_that("missing columns are refused", {
  x <- capm_fixture()
  expect_error(report_capm_alphas(x$spreads[, -7], x$factors), "net_return")
  expect_error(report_capm_alphas(x$spreads, x$factors[, 1]), "mkt_rf")
})
