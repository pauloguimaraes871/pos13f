#' Backtest every quantile portfolio of one signal with factoRverse
#'
#' Runs `run_port_backtest()` once per quantile of `signal` (quintiles by default) and
#' bundles the runs with `create_port_backtest_cohort()`. The full ladder, not just the
#' two extreme legs, shows whether returns rise monotonically with the signal; the
#' portfolio sort is a non-parametric estimate of that relation. Q1 holds the lowest
#' scores and Qn the highest; the long-short spread is Qn minus Q1.
#'
#' @param inputs Named list from `build_backtest_inputs()`.
#' @param signal Character scalar, a column of `inputs$signals_m_df`.
#' @param method Character scalar, factoRverse construction method: `"cs"` (signal score
#'   times liquidity), `"ew"` or `"rp"` (risk parity; sample covariance of the previous
#'   240 daily returns in `inputs$daily_returns_m_xts`).
#' @param n_quantiles Numeric scalar, number of portfolios.
#' @param and_rule Optional `meta_dataframe` of a factoRverse AND rule (5 columns, the
#'   last 0 or 1) restricting every portfolio, for example to one momentum tercile.
#' @param label Character scalar, prefix of the configuration names.
#' @param direct_cost Numeric scalar, factoRverse `direct_transaction_cost` in percent of
#'   each dollar traded (0.05 is 5 bps; factoRverse accepts values below 0.1).
#' @param verbose Logical; factoRverse progress messages.
#'
#' @return A `port_backtest_cohort` whose backtests are named `<label>_q1` to
#'   `<label>_q<n>`, each with the weighted average of `signal` as a portfolio metric.
#'
#' @details
#' The bottom portfolio is the short leg, so it is scored on the negated signal
#' (factoRverse position `"short"`): its top quantile of `-signal` is the bottom quantile
#' of `signal`, and under `cs` the weights then grow with conviction in the short
#' direction (most negative signal) times liquidity. Scored on the signal itself, `cs`
#' would put the largest short weights on the least negative names. The other portfolios
#' are scored on the signal. `ew` and `rp` weights do not use the score.
#'
#' Rebalancing in March, June, September and December (the holding months that start
#' after each formation), weights drift in between. Market impact is set negligible
#' (`alpha = 1e-6`): trading costs are flat per dollar traded, as pre-registered, and
#' scale linearly with `direct_cost`.
#' @export
run_quantile_ladder <- function(inputs, signal, method = "cs", n_quantiles = 5, and_rule = NULL,
                                label = paste(signal, method, sep = "_"), direct_cost = 0.05,
                                verbose = FALSE) {
  # Validate inputs
  if (!signal %in% names(inputs$signals_m_df@data)) {
    stop("signal is not a column of signals_m_df: ", signal, ".")
  }
  if (!method %in% c("cs", "ew", "rp")) stop("method must be one of cs, ew, rp.")

  # One configuration and run per quantile
  metric <- factoRverse::create_meta_dataframe(
    dplyr::select(inputs$signals_m_df@data, "id", "tickers", "dates", dplyr::all_of(signal)),
    meta_dataframe_name = signal
  )
  runs <- purrr::map(seq_len(n_quantiles), function(k) {
    ## The bottom portfolio is the top quantile of the negated signal
    short_leg <- k == 1
    range <- if (short_leg) c(1 - 1 / n_quantiles, 1) else c(k - 1, k) / n_quantiles
    position <- if (short_leg) "short" else "long"
    config <- factoRverse::create_port_backtest_config(
      chosen_score_metric_and_position = stats::setNames(position, signal),
      eligibility_quantile_range = range,
      initial_buffer_period = 1,
      rebalancing_months = c(3, 6, 9, 12), port_construction_method = method,
      main_liquidity_metric = "mean_volfin_3m", config_name = paste0(label, "_q", k),
      ## 240 trading days stay inside every member's life (365 calendar days of history)
      cov_est_method = factoRverse::create_cov_est_method(
        cov_matrix_sample_size = 240, active_returns = FALSE
      )
    ) |>
      factoRverse::add_transaction_costs_parameters(
        direct_transaction_cost = direct_cost, alpha = 1e-6, lambda = 1, strategy_aum = 1e6
      )
    muffle_known_warnings(factoRverse::run_port_backtest(
      inputs$signals_m_df, inputs$fwd_return_m_df, inputs$liquidity_m_df,
      inputs$volatility_m_df,
      config = config, custom_stock_metrics_m_df = metric,
      user_defined_AND_rules_m_df = and_rule,
      daily_stock_returns_m_xts = if (method == "rp") inputs$daily_returns_m_xts,
      verbose = verbose, parallel = FALSE
    ))
  })
  factoRverse::create_port_backtest_cohort(runs, cohort_name = label)
}
