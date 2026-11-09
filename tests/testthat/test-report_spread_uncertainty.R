## A spread with a zero mean (in the spreads table) and a benchmark (only in the summary),
## at two lags, as the summary pin holds them
uncertainty_fixture <- function() {
  months <- seq(as.Date("2015-01-01"), by = "month", length.out = 12)
  zero_mean <- rep(c(2, -1, -1), 4)
  list(
    summary = tibble::tibble(
      series = rep(c("f1|cs|unconditional|spread_0_0", "spy"), each = 2),
      lag = rep(c(3, 6), 2), n_months = 12, n_alpha_months = 11,
      mean_m = c(0, 0, 1, 1), t_mean = c(0, 0, 2, 1.8),
      alpha_se = c(0.15, 0.16, 0.1, 0.1)
    ),
    spreads = tibble::tibble(
      signal = "f1", method = "cs", design = "unconditional", cost_bps = 0,
      borrow_bps = 0, dates = months, net_return = zero_mean
    )
  )
}

test_that("a spread's interval comes from its returns, even with a zero mean", {
  x <- uncertainty_fixture()
  out <- report_spread_uncertainty(x$summary, x$spreads)
  fit <- stats::lm(x$spreads$net_return ~ 1)
  se <- sqrt(sandwich::NeweyWest(fit, lag = 3, prewhite = FALSE))[1]
  expect_equal(out$ann_return_hi[1], 12 * stats::qnorm(0.975) * se)
  expect_equal(out$mde_mean_ann[1], 12 * (stats::qnorm(0.975) + stats::qnorm(0.8)) * se)
  expect_equal(out$mde_alpha_ann[1], 12 * (stats::qnorm(0.975) + stats::qnorm(0.8)) * 0.15)
  ## Shuffled months give the same error
  shuffled <- x$spreads[withr::with_seed(1, sample(12)), ]
  expect_equal(
    report_spread_uncertainty(x$summary, shuffled)$mde_mean_ann[1], out$mde_mean_ann[1]
  )
})

test_that("other series recover the error from the t-statistic", {
  x <- uncertainty_fixture()
  out <- report_spread_uncertainty(x$summary, x$spreads)
  expect_equal(out$ann_return_lo[2], 12 * (1 - stats::qnorm(0.975) * 1 / 2))
  expect_equal(out$n_alpha_months[2], 11)
})

test_that("a missing lag or column is refused", {
  x <- uncertainty_fixture()
  expect_error(report_spread_uncertainty(x$summary, x$spreads, lag = 12), "no rows at lag")
  expect_error(report_spread_uncertainty(x$summary[, -6], x$spreads), "missing columns: t_mean")
  expect_error(report_spread_uncertainty(x$summary, x$spreads[, -7]), "net_return")
})
