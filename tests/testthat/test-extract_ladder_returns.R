test_that("every series is dated by its holding month and net equals raw minus costs", {
  ladder <- toy_ladder()
  expect_true(all(lubridate::mday(ladder$dates) == 1))
  ## Finished months only: August 2020 is the toy's unfinished month
  months <- seq(as.Date("2019-12-01"), as.Date("2020-07-01"), by = "month")
  expect_equal(sort(unique(ladder$dates)), months)
  finished <- dplyr::filter(ladder, !is.na(.data$raw_return))
  costs <- finished$direct_cost + finished$market_impact_cost
  expect_equal(finished$net_return, finished$raw_return - costs)
  ## Trades only at the formations: December, March, June
  ## Costs between rebalances are floating-point noise (about 1e-17)
  traded <- unique(ladder$dates[ladder$direct_cost > 1e-10])
  expect_true(all(lubridate::month(traded) %in% c(3, 6, 12)))
})

test_that("raw returns match weights times forward returns", {
  inputs <- toy_backtest_inputs()
  cohort <- run_quantile_ladder(inputs, "f1_breadth_change", "cs", n_quantiles = 2)
  ladder <- extract_ladder_returns(cohort)
  weights <- cohort@port_backtest_results_list[[2]]@port_weights_m_df@data
  manual <- weights |>
    dplyr::inner_join(inputs$fwd_return_m_df@data, by = dplyr::join_by("id", "tickers", "dates")) |>
    dplyr::summarise(r = sum(.data$eop_port_weights * .data$fwd_return_1m), .by = "dates") |>
    dplyr::filter(!is.na(.data$r)) |>
    dplyr::mutate(dates = as.Date(as.numeric(.data$dates)))
  top <- dplyr::filter(ladder, .data$quantile == 2, !is.na(.data$raw_return))
  expect_equal(top$raw_return, manual$r[match(top$dates, manual$dates)])
})
