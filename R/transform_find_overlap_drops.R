#' Filers dropped by the manager overlap rule (plan 1.4)
#'
#' A pair overlaps at formation t, for a compared quarter (q-1 or q), when filer A's
#' report for that quarter as it stands at t includes manager B (OTHERMANAGER2,
#' `included_in_report`) and B is itself a cohort candidate. Counting both would count
#' the same positions twice, so the smaller report of that quarter, by positions, leaves
#' the cohort for the formation; a tie drops B, the included manager. The matched cohort
#' compares both quarters, so a drop in either quarter removes the filer (plan 1.4: the
#' pair is dropped "for that quarter").
#'
#' @param candidates Tibble of cohort candidates: `formation_date`, `cik`, `n_prev`,
#'   `n_curr` (positions in q-1 and q).
#' @param formations Tibble from `build_formation_dates()`.
#' @param relations Tibble pinned as `silver_filing_relations`.
#' @param versions Tibble pinned as `silver_holdings_versions`.
#'
#' @return A tibble with `formation_date`, `cik` and `overlap` (`TRUE`), one row per
#'   dropped filer.
#'
#' @details
#' Point in time: relations are read only from the filings that make up A's report
#' version current at t (`list_current_accessions()`), so a link removed by a
#' restatement filed before t no longer counts, and a link added after t is not yet
#' seen. Related managers are matched by CIK with leading zeros removed; the 40% of
#' relations without a CIK cannot be matched and are left out.
#' @noRd
find_overlap_drops <- function(candidates, formations, relations, versions) {
  strip <- function(x) sub("^0+", "", trimws(x))
  included <- list_current_accessions(versions, formations) |>
    dplyr::inner_join(
      relations |>
        dplyr::filter(.data$relation == "included_in_report", !is.na(.data$related_cik)) |>
        dplyr::select("accession_number", "related_cik"),
      by = dplyr::join_by("accession_number"), relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      .data$formation_date, .data$quarter,
      a_key = strip(.data$cik),
      b_key = strip(.data$related_cik)
    ) |>
    dplyr::filter(.data$a_key != .data$b_key) |>
    dplyr::distinct()

  # Size of each candidate's report in each compared quarter
  sizes <- candidates |>
    dplyr::transmute(.data$formation_date, .data$cik,
      key = strip(.data$cik),
      prev = .data$n_prev, curr = .data$n_curr
    ) |>
    tidyr::pivot_longer(c("prev", "curr"), names_to = "quarter", values_to = "n")
  side <- function(prefix) {
    dplyr::rename_with(sizes, \(x) paste0(prefix, x), c("cik", "key", "n"))
  }
  included |>
    dplyr::inner_join(side("a_"),
      by = dplyr::join_by("formation_date", "quarter", "a_key"), relationship = "many-to-one"
    ) |>
    dplyr::inner_join(side("b_"),
      by = dplyr::join_by("formation_date", "quarter", "b_key"), relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      formation_date = .data$formation_date,
      cik = dplyr::if_else(.data$a_n >= .data$b_n, .data$b_cik, .data$a_cik)
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(overlap = TRUE)
}
