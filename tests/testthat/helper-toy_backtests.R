# Backtest fixtures on the gold toy build ----
#
# The toy gold build (helper-toy_gold.R) holds 8 universe members per formation over
# three formations (holding months 2019-12 to 2020-08, August unfinished). Its ladders
# use two quantiles, four stocks each.
toy_backtest_inputs <- function(env = parent.frame()) {
  build <- toy_gold_build(env)
  daily <- build$toy$returns_d |>
    dplyr::transmute(tickers = .data$instrument_id, dates = .data$date, ret = 100 * .data$ret)
  shape_backtest_inputs(
    signals = build$panels$signals_m_df, fwd = build$panels$fwd_1m_returns_m_df,
    liquidity = build$panels$liquidity_m_df, volatility = build$panels$volatility_m_df,
    daily = daily
  )
}

## A tidy two-quantile ladder of F1 under one method, from factoRverse
toy_ladder <- function(method = "cs", env = parent.frame()) {
  inputs <- toy_backtest_inputs(env)
  cohort <- run_quantile_ladder(inputs, "f1_breadth_change", method, n_quantiles = 2)
  extract_ladder_returns(cohort) |>
    dplyr::mutate(signal = "f1_breadth_change", method = method, design = "unconditional")
}
