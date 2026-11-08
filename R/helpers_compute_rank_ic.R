#' Rank information coefficient of a signal, one value per formation
#'
#' At each formation, the Spearman correlation between the signal and each stock's
#' return over the three holding months that follow (compounded from the monthly forward
#' returns of the frozen panel rows). Inference counts formations, not months (memo
#' Section 7), so the mean IC is tested with the standard error across formations.
#'
#' @param signals Tibble of gold panel rows (`tickers`, `dates`, `signal_age_months` and
#'   the signal column).
#' @param fwd Tibble pinned as `gold_fwd_1m_returns_m_df` (`id`, `fwd_return_1m`,
#'   percentage points).
#' @param signal Character scalar, the signal column.
#'
#' @return A list: `by_formation` (tibble with `formation` (Date, the first holding
#'   month), `ic` and `n_stocks`) and `summary` (tibble with `n_formations`, `mean_ic`,
#'   `se_ic`, `t_ic`, `ic_lo`, `ic_hi`).
#'
#' @details
#' Only formations whose three holding months are all finished enter. A stock whose
#' price stops inside the holding period keeps its booked returns (0 after the last
#' price), as in the portfolios.
#' @noRd
compute_rank_ic <- function(signals, fwd, signal) {
  rows <- signals |>
    dplyr::select("id", "tickers", "dates", "signal_age_months", value = dplyr::all_of(signal)) |>
    dplyr::inner_join(dplyr::select(fwd, "id", "fwd_return_1m"),
      by = dplyr::join_by("id"), relationship = "one-to-one"
    ) |>
    dplyr::mutate(formation = formation_label_of(.data$dates, .data$signal_age_months))
  by_formation <- rows |>
    dplyr::summarise(
      value = dplyr::first(.data$value),
      ret_3m = 100 * (prod(1 + .data$fwd_return_1m / 100) - 1),
      n_months = dplyr::n(),
      .by = c("formation", "tickers")
    ) |>
    ## A stock with an unfinished month has NA; a formation counts only when complete
    dplyr::filter(.data$n_months == 3) |>
    dplyr::summarise(
      complete = !anyNA(.data$ret_3m),
      ic = stats::cor(.data$value, .data$ret_3m, method = "spearman"),
      n_stocks = dplyr::n(),
      .by = "formation"
    ) |>
    dplyr::filter(.data$complete) |>
    dplyr::select(-"complete") |>
    dplyr::arrange(.data$formation)
  n <- nrow(by_formation)
  se <- stats::sd(by_formation$ic) / sqrt(n)
  list(
    by_formation = by_formation,
    summary = tibble::tibble(
      n_formations = n, mean_ic = mean(by_formation$ic), se_ic = se,
      t_ic = mean(by_formation$ic) / se,
      ic_lo = mean(by_formation$ic) - 1.96 * se, ic_hi = mean(by_formation$ic) + 1.96 * se
    )
  )
}
