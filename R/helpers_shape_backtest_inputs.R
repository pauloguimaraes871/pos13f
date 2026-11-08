#' Shape gold panels into the meta objects `run_port_backtest()` consumes
#'
#' Adds derived signal columns and wraps everything as factoRverse meta objects. Every
#' signal the backtests may sort on is a column of `signals_m_df`, so a new factor (F1b,
#' a blend, a conditional rank) needs a column, not new backtest code.
#'
#' @param signals,fwd,liquidity,volatility Tibbles of the gold panels in meta_dataframe
#'   layout (`id`, `tickers`, `dates` first).
#' @param daily Tibble of daily returns (`tickers`, `dates`, `ret` in percentage points).
#' @param double_sorts List of character pairs `c(signal, conditioner)` for which
#'   `add_conditional_rank()` adds a within-tercile rank.
#' @param fwd_col Character scalar, the forward-return column backtested as
#'   `fwd_return_1m`: `"fwd_return_1m"` (baseline) or `"fwd_return_1m_stress"` (the
#'   month of a stock's last price compounded with -30%, plan 1.6).
#'
#' @return A named list: `signals_m_df`, `fwd_return_m_df` (`fwd_return_1m` only),
#'   `liquidity_m_df`, `volatility_m_df` (`meta_dataframe`), `daily_returns_m_xts`
#'   (`meta_xts`, one column per instrument) and `signals` (the plain tibble).
#'
#' @details
#' Dates read from parquet can be integer-backed; factoRverse compares `current_date`
#' values with `unique()` on a list, which treats integer and double Dates as different,
#' so every date is stored as double.
#'
#' factoRverse estimates covariances from daily returns dated up to and including the
#' rebalancing row's date, the first day of the holding month, while the formation is
#' at the close of the last trading day before it. When that first day is a trading day
#' its return is after formation, so returns dated on the first of a month are left out
#' of the covariance input (about 5% of days; no other look-ahead is possible). Daily
#' returns after the panels' last date are dropped too: factoRverse requires every input
#' to share `current_date`.
#'
#' factoRverse needs a covariance window without gaps. Inside an instrument's life, a
#' day without a price row gets a return of 0: silver books the move across a gap on the
#' next traded day (close over the last available close), so the missing day's own return
#' is exactly 0 and the compounded return is unchanged. Days before the first or after
#' the last price stay missing; `run_quantile_ladder()` uses a 240-day window, inside
#' every member's life (members need 365 calendar days of history). This fill is for
#' the covariance input only: it fabricates no row of any panel.
#' @noRd
shape_backtest_inputs <- function(signals, fwd, liquidity, volatility, daily,
                                  double_sorts = list(c("f1_breadth_change", "mom_12_1")),
                                  fwd_col = "fwd_return_1m") {
  if (!fwd_col %in% c("fwd_return_1m", "fwd_return_1m_stress")) {
    stop("fwd_col must be fwd_return_1m or fwd_return_1m_stress.")
  }
  as_double_date <- function(df) dplyr::mutate(df, dates = as.Date(as.numeric(.data$dates)))
  signals <- as_double_date(signals)
  for (pair in double_sorts) signals <- add_conditional_rank(signals, pair[1], pair[2])

  # Daily returns of members, wide, without returns dated on the first of a month and
  # none after the panels' last date
  last_date <- max(signals$dates)
  wide <- as_double_date(daily) |>
    dplyr::filter(lubridate::mday(.data$dates) != 1, .data$dates <= last_date) |>
    dplyr::select("dates", "tickers", "ret") |>
    tidyr::pivot_wider(names_from = "tickers", values_from = "ret") |>
    dplyr::arrange(.data$dates) |>
    dplyr::mutate(dplyr::across(-"dates", fill_inner_gaps))
  daily_xts <- xts::xts(as.matrix(dplyr::select(wide, -"dates")), order.by = wide$dates)
  ## factoRverse requires every input to share current_date; a meta_xts takes its last
  ## index, so it is set to the panels' last date (no daily return lies after it)
  daily_m_xts <- factoRverse::create_meta_xts(daily_xts, meta_xts_name = "gold_daily_returns_d")
  daily_m_xts@current_date <- last_date

  meta <- function(df, name, ...) {
    factoRverse::create_meta_dataframe(as.data.frame(as_double_date(df)),
      meta_dataframe_name = name, ...
    )
  }
  list(
    signals_m_df = meta(signals, "gold_signals_m_df", type = "signals"),
    fwd_return_m_df = meta(
      dplyr::select(fwd, "id", "tickers", "dates", fwd_return_1m = dplyr::all_of(fwd_col)),
      "gold_fwd_1m_returns_m_df",
      type = "target"
    ),
    liquidity_m_df = meta(liquidity, "gold_liquidity_m_df"),
    volatility_m_df = meta(volatility, "gold_volatility_m_df"),
    daily_returns_m_xts = daily_m_xts,
    signals = signals
  )
}
