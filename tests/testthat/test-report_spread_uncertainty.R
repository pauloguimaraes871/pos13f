## Two series at two lags, as the summary pin holds them; the second has a zero mean
uncertainty_fixture <- function() {
  tibble::tibble(
    series = rep(c("f1|cs|unconditional|spread_25_100", "flat"), each = 2),
    lag = rep(c(3, 6), 2), n_months = 152, n_alpha_months = 151,
    mean_m = c(0.06, 0.06, 0, 0), t_mean = c(0.2, 0.18, 0, 0),
    alpha_se = c(0.15, 0.16, 0.1, 0.1)
  )
}

test_that("the interval and both detectable effects follow from the Newey-West errors", {
  out <- report_spread_uncertainty(uncertainty_fixture())
  expect_equal(nrow(out), 2)
  se <- 0.06 / 0.2
  expect_equal(out$ann_return_lo[1], 12 * (0.06 - stats::qnorm(0.975) * se))
  expect_equal(out$ann_return_hi[1], 12 * (0.06 + stats::qnorm(0.975) * se))
  expect_equal(out$mde_mean_ann[1], 12 * (stats::qnorm(0.975) + stats::qnorm(0.8)) * se)
  expect_equal(out$mde_alpha_ann[1], 12 * (stats::qnorm(0.975) + stats::qnorm(0.8)) * 0.15)
  expect_equal(out$n_alpha_months[1], 151)
})

test_that("a zero mean gives no interval, and a missing lag or column is refused", {
  out <- report_spread_uncertainty(uncertainty_fixture())
  expect_true(is.na(out$ann_return_lo[2]))
  expect_error(report_spread_uncertainty(uncertainty_fixture(), lag = 12), "no rows at lag")
  expect_error(report_spread_uncertainty(uncertainty_fixture()[, -6]), "missing columns: t_mean")
})
