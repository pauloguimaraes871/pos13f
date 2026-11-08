## Two months of a three-quantile ladder; the run paid 5 bps per dollar traded
spread_ladder <- function() {
  tibble::tibble(
    dates = rep(as.Date(c("2020-03-01", "2020-04-01")), each = 3), quantile = rep(1:3, 2),
    raw_return = c(1, 2, 4, -1, 0, 2), direct_cost = c(0.05, 0.05, 0.1, 0, 0, 0)
  )
}

test_that("the spread is top minus bottom, with costs rescaled and borrow charged", {
  out <- compute_spread_returns(spread_ladder(), cost_bps = c(0, 25), borrow_bps = c(0, 120))
  base <- dplyr::filter(out, .data$cost_bps == 0, .data$borrow_bps == 0)
  expect_equal(base$gross_return, c(3, 3))
  ## 25 bps is five times the run's fee: (0.1 + 0.05) x 5 in March
  costly <- dplyr::filter(out, .data$cost_bps == 25, .data$borrow_bps == 0)
  expect_equal(costly$trading_cost, c(0.75, 0))
  ## 120 bps a year is 0.1 percentage points a month on the short leg
  borrowed <- dplyr::filter(out, .data$cost_bps == 25, .data$borrow_bps == 120)
  expect_equal(borrowed$net_return, c(3 - 0.75 - 0.1, 3 - 0.1))
})

test_that("a ladder with duplicated months or missing columns is refused", {
  doubled <- dplyr::bind_rows(spread_ladder(), spread_ladder()[1, ])
  expect_error(compute_spread_returns(doubled), "one row per")
  no_cost <- dplyr::select(spread_ladder(), -"direct_cost")
  expect_error(compute_spread_returns(no_cost), "missing columns")
})
