#' Rank autocorrelation of a signal between consecutive formations
#'
#' A flow signal that changes completely each quarter forces high turnover; this
#' measures how much of the cross-sectional ranking survives one formation.
#'
#' @param signals Tibble pinned as `gold_signals_m_df` (`tickers`, `dates`,
#'   `signal_age_months` and the signal).
#' @param signal Character scalar, the signal column.
#'
#' @return A tibble with `formation` (Date, first holding month), `n` (stocks in both
#'   formations) and `rank_autocorrelation` (Spearman correlation with the previous
#'   formation).
#' @noRd
report_signal_persistence <- function(signals, signal = "f1_breadth_change") {
  at_formation <- signals |>
    dplyr::filter(.data$signal_age_months == 0) |>
    dplyr::select("tickers", "dates", value = dplyr::all_of(signal))
  formations <- sort(unique(at_formation$dates))
  purrr::map(seq_along(formations)[-1], function(i) {
    pair <- dplyr::inner_join(
      dplyr::filter(at_formation, .data$dates == formations[i - 1]),
      dplyr::filter(at_formation, .data$dates == formations[i]),
      by = dplyr::join_by("tickers"), relationship = "one-to-one"
    )
    tibble::tibble(
      formation = formations[i], n = nrow(pair),
      rank_autocorrelation = stats::cor(pair$value.x, pair$value.y, method = "spearman")
    )
  }) |>
    dplyr::bind_rows()
}
