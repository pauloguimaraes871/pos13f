#' Add a signal's percentile rank within groups of a conditioning variable
#'
#' Prepares a dependent double sort for `run_quantile_ladder()`: at each date, stocks
#' are split into `n_groups` equal groups by `conditioner` (for example momentum
#' terciles), and `signal` is ranked within its group. A quantile cut on the rank column
#' combined with a group AND rule then selects, inside each group, the same share of
#' stocks as an unconditional cut would select from the whole universe. factoRverse
#' applies its quantile cut to the whole universe before AND rules, so a cut on the raw
#' signal would give an independent sort instead.
#'
#' @param signals Tibble in meta_dataframe layout (`id`, `tickers`, `dates`, signal
#'   columns), one row per stock and month.
#' @param signal Character scalar, the column to rank.
#' @param conditioner Character scalar, the column that defines the groups.
#' @param n_groups Numeric scalar, number of groups (3 for terciles).
#'
#' @return `signals` with two more numeric columns: `<signal>_in_<conditioner>` (the
#'   percentile rank of `signal` within its group and date, in (0, 1]) and
#'   `<conditioner>_group` (1 for the lowest group to `n_groups`).
#'
#' @details
#' Groups and ranks use only values at the same date, so they carry no look-ahead. Ties
#' are broken by `tickers`, as in the universe ranking.
#' @noRd
add_conditional_rank <- function(signals, signal, conditioner, n_groups = 3) {
  # Validate inputs
  missing_cols <- setdiff(c("tickers", "dates", signal, conditioner), names(signals))
  if (length(missing_cols) > 0) {
    stop("signals is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  if (anyNA(signals[[signal]]) || anyNA(signals[[conditioner]])) {
    stop(
      "signals$", signal, " and signals$", conditioner,
      " must not be NA: a missing value has no group."
    )
  }

  # Groups by the conditioner, then ranks within each group and date
  rank_col <- paste0(signal, "_in_", conditioner)
  group_col <- paste0(conditioner, "_group")
  signals |>
    dplyr::arrange(.data$dates, .data$tickers) |>
    dplyr::mutate(
      !!group_col := as.numeric(dplyr::ntile(.data[[conditioner]], n_groups)),
      .by = "dates"
    ) |>
    dplyr::mutate(
      !!rank_col := dplyr::row_number(.data[[signal]]) / dplyr::n(),
      .by = c("dates", dplyr::all_of(group_col))
    ) |>
    dplyr::arrange(.data$id)
}
