test_that("inputs are meta objects with double dates and no first-of-month daily returns", {
  inputs <- toy_backtest_inputs()
  expect_s4_class(inputs$signals_m_df, "meta_dataframe")
  expect_s4_class(inputs$daily_returns_m_xts, "meta_xts")
  expect_true(is.double(unclass(inputs$signals_m_df@data$dates)))
  days <- as.Date(as.POSIXct(xts::.index(inputs$daily_returns_m_xts@data), tz = "UTC"))
  expect_false(any(lubridate::mday(days) == 1))
  expect_true("f1_breadth_change_in_mom_12_1" %in% names(inputs$signals_m_df@data))
  expect_equal(names(inputs$fwd_return_m_df@data), c("id", "tickers", "dates", "fwd_return_1m"))
})

test_that("the stress scenario backtests the stressed forward returns", {
  build <- toy_gold_build()
  args <- list(
    signals = build$panels$signals_m_df, fwd = build$panels$fwd_1m_returns_m_df,
    liquidity = build$panels$liquidity_m_df, volatility = build$panels$volatility_m_df,
    daily = dplyr::transmute(build$toy$returns_d,
      tickers = .data$instrument_id, dates = .data$date, ret = 100 * .data$ret
    )
  )
  stressed <- do.call(shape_backtest_inputs, c(args, fwd_col = "fwd_return_1m_stress"))
  expect_equal(
    stressed$fwd_return_m_df@data$fwd_return_1m,
    build$panels$fwd_1m_returns_m_df$fwd_return_1m_stress
  )
  expect_error(do.call(shape_backtest_inputs, c(args, fwd_col = "other")), "fwd_col must be")
})
