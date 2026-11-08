#' Cohort size, universe size and exclusion reasons per formation
#'
#' @param universe Tibble pinned as `gold_universe_diagnostics`.
#' @param cohort Tibble pinned as `gold_cohort`.
#'
#' @return A list: `sizes` (tibble with `formation_date`, `n_candidates`, `n_members` and
#'   `cohort_size`) and `reasons` (tibble with `formation_date`, `reason`, `"member"` for
#'   members, and `n`).
#' @noRd
report_universe_composition <- function(universe, cohort) {
  sizes <- universe |>
    dplyr::summarise(
      n_candidates = dplyr::n(), n_members = sum(.data$in_universe), .by = "formation_date"
    ) |>
    dplyr::left_join(
      dplyr::summarise(cohort, cohort_size = sum(.data$in_cohort), .by = "formation_date"),
      by = dplyr::join_by("formation_date"), relationship = "one-to-one"
    ) |>
    dplyr::arrange(.data$formation_date)
  reasons <- universe |>
    dplyr::mutate(reason = dplyr::coalesce(.data$excluded_reason, "member")) |>
    dplyr::summarise(n = dplyr::n(), .by = c("formation_date", "reason"))
  list(sizes = sizes, reasons = reasons)
}
