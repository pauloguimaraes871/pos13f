#' Read the gold panels and shape them for `run_port_backtest()`
#'
#' Reads the four gold panels and the daily returns of universe members from the board
#' and hands them to `shape_backtest_inputs()`.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param double_sorts List of character pairs `c(signal, conditioner)`, passed on.
#'
#' @return The list of `shape_backtest_inputs()`.
#' @noRd
build_backtest_inputs <- function(board, double_sorts = list(c("f1_breadth_change", "mom_12_1"))) {
  read <- function(name) tibble::as_tibble(pins::pin_read(board, name))
  ## Uploaded as a parquet file by write_gold_pins() (millions of rows)
  daily_path <- pins::pin_download(board, "gold_daily_returns_d")
  daily <- tibble::as_tibble(nanoparquet::read_parquet(daily_path))
  shape_backtest_inputs(
    signals = read("gold_signals_m_df"), fwd = read("gold_fwd_1m_returns_m_df"),
    liquidity = read("gold_liquidity_m_df"), volatility = read("gold_volatility_m_df"),
    daily = daily, double_sorts = double_sorts
  )
}
