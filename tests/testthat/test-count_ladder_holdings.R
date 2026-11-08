test_that("holdings count the stocks with positive weight in each month", {
  inputs <- toy_backtest_inputs()
  cohort <- run_quantile_ladder(inputs, "f1_breadth_change", "ew", n_quantiles = 2)
  holdings <- count_ladder_holdings(cohort)
  expect_setequal(unique(holdings$quantile), 1:2)
  weights <- cohort@port_backtest_results_list[[1]]@port_weights_m_df@data
  direct <- dplyr::summarise(weights, n = sum(.data$eop_port_weights > 0), .by = "dates")
  q1 <- dplyr::filter(holdings, .data$quantile == 1)
  expect_equal(q1$n_holdings, direct$n[match(q1$dates, as.Date(as.numeric(direct$dates)))])
  ## Eight members split in two halves; factoRverse's cuts are inclusive, so a stock
  ## whose F1 ties the median sits in both halves (the toy cohort has many zero changes)
  expect_true(all(holdings$n_holdings >= 4 & holdings$n_holdings <= 6))
})
