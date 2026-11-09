test_that("turnover, the cost decomposition and yearly costs follow from the ladder", {
  ladder <- report_ladder_fixture()
  spreads <- compute_spread_returns(dplyr::filter(ladder, !is.na(.data$raw_return))) |>
    dplyr::mutate(signal = "f1", method = "cs", design = "unconditional")
  out <- report_cost_summary(ladder, spreads)
  ## After the first month, Q1 traded 0.04 / 0.0005 = 0.8 of its value, Q2 traded 1.2
  expect_equal(out$turnover$traded_per_rebalance, c(0.8, 1.2))
  ## The same trades, one row per leg and rebalance
  expect_equal(out$by_rebalance$traded, c(0.8, 1.2))
  expect_equal(out$by_rebalance$dates, rep(as.Date("2019-06-01"), 2))
  base <- dplyr::filter(out$decomposition, .data$cost_bps == 25, .data$borrow_bps == 100)
  expect_equal(base$net, base$gross - base$trading - base$borrow)
  expect_equal(out$by_year$year, 2019)
})
