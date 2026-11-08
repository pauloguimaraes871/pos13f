test_that("each quantile runs as its own factoRverse backtest, sorted on the signal", {
  inputs <- toy_backtest_inputs()
  cohort <- run_quantile_ladder(inputs, "f1_breadth_change", "cs", n_quantiles = 2)
  expect_s4_class(cohort, "port_backtest_cohort")
  expect_length(cohort@port_backtest_results_list, 2)
  ladder <- extract_ladder_returns(cohort)
  ## The top half holds the higher F1 in every month
  wide <- tidyr::pivot_wider(ladder,
    id_cols = "dates", names_from = "quantile", values_from = "signal_level"
  )
  expect_true(all(wide$`2` > wide$`1`))
})

test_that("ew and rp ladders run on the toy, and bad arguments are refused", {
  inputs <- toy_backtest_inputs()
  for (method in c("ew", "rp")) {
    cohort <- run_quantile_ladder(inputs, "f1_breadth_change", method, n_quantiles = 2)
    ladder <- extract_ladder_returns(cohort)
    expect_false(anyNA(ladder$raw_return[ladder$dates < as.Date("2020-08-01")]))
  }
  expect_error(run_quantile_ladder(inputs, "no_such_signal"), "not a column")
  expect_error(run_quantile_ladder(inputs, "f1_breadth_change", "mvo"), "one of cs, ew, rp")
})

test_that("under cs the bottom portfolio weights the most negative signal most", {
  inputs <- toy_backtest_inputs()
  cohort <- run_quantile_ladder(inputs, "f1_breadth_change", "cs", n_quantiles = 2)
  weights <- cohort@port_backtest_results_list[[1]]@port_weights_m_df@data |>
    dplyr::filter(.data$eop_port_weights > 0) |>
    dplyr::inner_join(dplyr::select(inputs$signals_m_df@data, "id", "f1_breadth_change"),
      by = dplyr::join_by("id")
    )
  ## Within each formation month, weight falls as F1 rises (liquidity aside)
  first_months <- dplyr::filter(weights, lubridate::month(.data$dates) %in% c(3, 6, 12))
  by_month <- dplyr::summarise(first_months,
    rho = stats::cor(.data$eop_port_weights, .data$f1_breadth_change, method = "spearman"),
    .by = "dates"
  )
  expect_true(mean(by_month$rho, na.rm = TRUE) < 0)
})
