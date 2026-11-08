#' Number of stocks held by each portfolio of a ladder in each month
#'
#' @param cohort A `port_backtest_cohort` from `run_quantile_ladder()`.
#'
#' @return A tibble with `quantile` (integer), `dates` (first day of the holding month)
#'   and `n_holdings` (stocks with a positive end-of-period weight).
#' @noRd
count_ladder_holdings <- function(cohort) {
  purrr::map(cohort@port_backtest_results_list, function(result) {
    k <- as.integer(stringr::str_match(result@backtest_identifier, "_q([0-9]+)")[, 2])
    result@port_weights_m_df@data |>
      dplyr::summarise(n_holdings = sum(.data$eop_port_weights > 0), .by = "dates") |>
      dplyr::mutate(quantile = k, dates = as.Date(as.numeric(.data$dates)))
  }) |>
    dplyr::bind_rows() |>
    dplyr::select("quantile", "dates", "n_holdings")
}
