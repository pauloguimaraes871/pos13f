test_that("break-even cost, MDE and PSR follow from the spread", {
  months <- seq(as.Date("2015-01-01"), by = "month", length.out = 60)
  spreads <- compute_spread_returns(tibble::tibble(
    dates = rep(months, each = 2), quantile = rep(1:2, 60),
    raw_return = rep(c(0, 1), 60) + rep(c(0.5, -0.5, 0.2, 0.1), 30),
    direct_cost = rep(c(0.05, 0.05, 0, 0, 0, 0), 20)
  ))
  out <- compute_spread_inference(spreads)
  gross <- dplyr::filter(spreads, .data$cost_bps == 0, .data$borrow_bps == 0)$gross_return
  scen <- dplyr::filter(spreads, .data$cost_bps == 10, .data$borrow_bps == 0)
  expect_equal(out$breakeven_bps, mean(gross) / mean(scen$trading_cost / 10))
  expect_gt(out$mde_ann, 0)
  expect_true(out$psr_iid > 0 && out$psr_iid < 1)
})
