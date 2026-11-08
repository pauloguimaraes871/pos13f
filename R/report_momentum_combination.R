#' Does adding F1 to momentum improve it? A descriptive comparison
#'
#' Compares the momentum spread alone with an equal-weight combination of the momentum
#' and F1 spreads (both gross, cs weights): their correlation, and the annualised mean,
#' volatility and Sharpe ratio of each. A combination improves on momentum alone when
#' F1 adds return that momentum does not already carry. This is descriptive, not one of
#' the pre-registered tests.
#'
#' @param spreads Tibble pinned as `backtests_spreads` (with the `reference` design).
#' @param signal Character scalar, the tested signal.
#' @param reference Character scalar, the reference signal (momentum).
#'
#' @return A list: `correlation` (numeric) and `table` (tibble with `portfolio`,
#'   `ann_return`, `ann_vol` and `sharpe`, for momentum, F1 and the 50/50 combination).
#' @noRd
report_momentum_combination <- function(spreads, signal = "f1_breadth_change",
                                        reference = "mom_12_1") {
  gross <- dplyr::filter(spreads, .data$cost_bps == 0, .data$borrow_bps == 0, .data$method == "cs")
  pick <- function(sig, design) {
    ## .env$: a bare `design` would resolve to the column and keep every design
    dplyr::filter(gross, .data$signal == .env$sig, .data$design == .env$design) |>
      dplyr::select("dates", "gross_return")
  }
  both <- dplyr::inner_join(pick(reference, "reference"), pick(signal, "unconditional"),
    by = dplyr::join_by("dates"), relationship = "one-to-one", suffix = c("_mom", "_f1")
  )
  stats_of <- function(name, r) {
    tibble::tibble(
      portfolio = name, ann_return = 12 * mean(r), ann_vol = sqrt(12) * stats::sd(r),
      sharpe = sqrt(12) * mean(r) / stats::sd(r)
    )
  }
  list(
    correlation = stats::cor(both$gross_return_mom, both$gross_return_f1),
    table = dplyr::bind_rows(
      stats_of("Momentum alone", both$gross_return_mom),
      stats_of("F1 alone", both$gross_return_f1),
      stats_of("50% momentum + 50% F1", (both$gross_return_mom + both$gross_return_f1) / 2)
    )
  )
}
