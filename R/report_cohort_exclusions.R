#' Filers removed from the cohort by reason: their number and their share of 13F value
#'
#' The cohort rules remove filers with fewer than 10 positions, index-like filers and the
#' smaller of two overlapping reports. F1 counts every filer once, so a rule's effect on
#' the signal depends on how many holder observations it removes, measured here by the
#' share of quarter-q positions, while its share of 13F dollar value shows how much of
#' institutional money it sets aside. The share of filers alone says neither.
#'
#' @param cohort Tibble pinned as `gold_cohort`: one row per `formation_date` and `cik`,
#'   with `n_curr` (positions in quarter q), `value_curr` (USD, quarter q holdings) and
#'   `excluded_reason` (`NA` for cohort
#'   members, `"few_positions"`, `"index_like"` or `"overlap"`).
#'
#' @return A tibble with one row per outcome (`"member"` and each reason): `outcome`,
#'   `mean_filers` (average count per formation), `share_filers`, `share_positions` and
#'   `share_value` (average shares per formation of filers, of quarter-q positions and of
#'   13F value).
#' @noRd
report_cohort_exclusions <- function(cohort) {
  # Validate inputs
  required_cols <- c("formation_date", "n_curr", "value_curr", "excluded_reason")
  missing_cols <- setdiff(required_cols, names(cohort))
  if (length(missing_cols) > 0) {
    stop("cohort is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }

  n_formations <- dplyr::n_distinct(cohort$formation_date)
  cohort |>
    dplyr::mutate(outcome = dplyr::coalesce(.data$excluded_reason, "member")) |>
    dplyr::summarise(
      n = dplyr::n(), positions = sum(.data$n_curr, na.rm = TRUE),
      value = sum(.data$value_curr, na.rm = TRUE),
      .by = c("formation_date", "outcome")
    ) |>
    ## Shares within each formation first, then averaged over every formation: a reason
    ## absent from a formation counts as zero there, not as a missing observation
    dplyr::mutate(
      share_filers = .data$n / sum(.data$n),
      share_positions = .data$positions / sum(.data$positions),
      share_value = .data$value / sum(.data$value),
      .by = "formation_date"
    ) |>
    dplyr::summarise(
      mean_filers = sum(.data$n) / .env$n_formations,
      share_filers = sum(.data$share_filers) / .env$n_formations,
      share_positions = sum(.data$share_positions) / .env$n_formations,
      share_value = sum(.data$share_value) / .env$n_formations, .by = "outcome"
    ) |>
    dplyr::arrange(dplyr::desc(.data$share_filers))
}
