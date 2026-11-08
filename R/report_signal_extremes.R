#' The stocks with the highest and lowest F1 at the last formation
#'
#' Shows what the signal picks: holder counts before and after, and liquidity, so the
#' reader can check that extreme F1 values are large changes in breadth and see the
#' size tilt of the definition.
#'
#' @param signals Tibble pinned as `gold_signals_m_df`.
#' @param master Tibble pinned as `silver_security_master` (`instrument_id`, `ticker`).
#' @param n Numeric scalar, stocks per side.
#'
#' @return A tibble with `side` (`"top"` or `"bottom"`), `ticker`, `f1_breadth_change`,
#'   `n_holders_prev`, `n_holders_curr` and `cohort_size`, at the last formation.
#' @noRd
report_signal_extremes <- function(signals, master, n = 10) {
  tickers <- master |>
    dplyr::distinct(.data$instrument_id, .data$ticker) |>
    dplyr::distinct(.data$instrument_id, .keep_all = TRUE)
  last_formation <- max(signals$dates[signals$signal_age_months == 0])
  last <- signals |>
    dplyr::filter(.data$signal_age_months == 0, .data$dates == last_formation) |>
    dplyr::left_join(tickers,
      by = dplyr::join_by("tickers" == "instrument_id"), relationship = "many-to-one"
    )
  pick <- function(side, ord) {
    dplyr::slice_head(dplyr::arrange(last, ord), n = n) |> dplyr::mutate(side = side)
  }
  dplyr::bind_rows(
    pick("top", dplyr::desc(last$f1_breadth_change)), pick("bottom", last$f1_breadth_change)
  ) |>
    dplyr::select(
      "side", "dates", "ticker", "f1_breadth_change", "n_holders_prev", "n_holders_curr",
      "cohort_size"
    )
}
