#' Signal columns of the gold panel: F1 and its controls, frozen at formation
#'
#' @param rows Tibble of panel rows from `build_gold_panels()` (breadth counts,
#'   `cohort_size`, features at formation, `signal_age_months`, and the key columns).
#' @param key Character vector, the key columns (`id`, `tickers`, `dates`).
#'
#' @return A tibble with the key columns then numeric columns: `f1_breadth_change`
#'   (the pre-registered signal, memo Section 3), `breadth_level`, `n_holders_prev`,
#'   `n_holders_curr`, `n_initiations`, `n_exits`, `cohort_size` (filers), `mom_12_1`,
#'   `rev_1m`, `vol_3m` (percentage points), `amihud` (per million USD) and
#'   `signal_age_months` (0 in the formation month).
#'
#' @details
#' `f1_breadth_change = (n_holders_curr - n_holders_prev) / cohort_size`, the matched
#' change in breadth of Chen, Hong and Stein (2002, p. 180) [src], who divide by all
#' funds in the sample at q-1 instead of the matched cohort; within a formation the two
#' differ by one factor, so cross-sectional ranks are the same (memo deviation D10).
#' `n_initiations` and `n_exits` are their IN and OUT decomposition, in counts. Only F1
#' is a tested hypothesis; the other columns are controls and diagnostics (owner
#' decision 2026-11-07), so they add no trials.
#' @noRd
gold_signal_columns <- function(rows, key) {
  rows |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(key)),
      f1_breadth_change = (.data$n_curr - .data$n_prev) / .data$cohort_size,
      breadth_level = .data$n_curr / .data$cohort_size,
      n_holders_prev = as.numeric(.data$n_prev),
      n_holders_curr = as.numeric(.data$n_curr),
      n_initiations = as.numeric(.data$n_init),
      n_exits = as.numeric(.data$n_exit),
      cohort_size = as.numeric(.data$cohort_size),
      mom_12_1 = .data$mom_12_1, rev_1m = .data$rev_1m, vol_3m = .data$vol_3m,
      amihud = .data$amihud, signal_age_months = .data$signal_age_months
    )
}
