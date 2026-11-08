#' Select the filer cohort of each formation
#'
#' A filer enters the cohort at formation t when its reports for both q-1 and q are
#' public at t (plan 1.2), it holds at least `min_positions` in each, it is not
#' index-like (plan 1.9: top tercile of positions and bottom tercile of turnover, both
#' recomputed per formation), and it is not the smaller side of an overlapping pair
#' (plan 1.4: a filer included in another filer's report for q-1 or q, as that report
#' stands at t, that also files alone; `find_overlap_drops()`).
#'
#' @param formations Tibble from `build_formation_dates()`.
#' @param versions Tibble pinned as `silver_holdings_versions` (`cik`,
#'   `period_of_report`, `accession_number`, `kind`, `event_no`, `base_no`, `valid_from`,
#'   `valid_to`, `is_visible`).
#' @param activity Tibble from `sql_gold_filer_activity()` (`formation_date`, `cik`,
#'   `n_prev`, `n_curr`, `n_both`, `value_curr`).
#' @param relations Tibble pinned as `silver_filing_relations`.
#' @param min_positions Numeric scalar, fewest positions in each quarter (plan 1.9: 10).
#'
#' @return A tibble with one row per formation and filer that had both reports public
#'   at t, ordered by `formation_date` and `cik`: the activity counts, `turnover`
#'   (share of positions initiated or exited between q-1 and q), `excluded_reason`
#'   (`NA`, `"few_positions"`, `"index_like"` or `"overlap"`) and `in_cohort` (lgl).
#' @noRd
select_gold_cohort <- function(formations, versions, activity, relations, min_positions = 10) {
  # Filers with both reports public at t (the version current at t, visible only)
  current <- function(period_col) {
    versions |>
      dplyr::filter(.data$is_visible) |>
      dplyr::inner_join(
        dplyr::select(formations, "formation_date", period_of_report = dplyr::all_of(period_col)),
        by = dplyr::join_by("period_of_report"), relationship = "many-to-many"
      ) |>
      dplyr::filter(
        .data$valid_from <= .data$formation_date,
        is.na(.data$valid_to) | .data$valid_to > .data$formation_date
      ) |>
      dplyr::distinct(.data$formation_date, .data$cik)
  }
  both <- dplyr::inner_join(current("prev_quarter"), current("report_quarter"),
    by = dplyr::join_by("formation_date", "cik"), relationship = "one-to-one"
  )

  # Counts and turnover; a filer with empty reports has no activity row
  cohort <- both |>
    dplyr::left_join(activity,
      by = dplyr::join_by("formation_date", "cik"), relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      dplyr::across(c("n_prev", "n_curr", "n_both", "value_curr"), \(x) dplyr::coalesce(x, 0)),
      turnover = (.data$n_prev + .data$n_curr - 2 * .data$n_both) /
        pmax(.data$n_prev + .data$n_curr - .data$n_both, 1),
      few = pmin(.data$n_prev, .data$n_curr) < min_positions
    )

  # Index-like: terciles recomputed per formation among filers with enough positions
  cohort <- cohort |>
    dplyr::mutate(
      index_like = !.data$few &
        .data$n_curr >= stats::quantile(.data$n_curr[!.data$few], 2 / 3, names = FALSE) &
        .data$turnover <= stats::quantile(.data$turnover[!.data$few], 1 / 3, names = FALSE),
      .by = "formation_date"
    )

  # Overlap: the smaller report of a pair leaves the cohort for that formation
  candidates <- dplyr::filter(cohort, !.data$few, !.data$index_like)
  dropped <- find_overlap_drops(candidates, formations, relations, versions)
  cohort |>
    dplyr::left_join(dropped,
      by = dplyr::join_by("formation_date", "cik"), relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      excluded_reason = dplyr::case_when(
        .data$few ~ "few_positions",
        .data$index_like ~ "index_like",
        dplyr::coalesce(.data$overlap, FALSE) ~ "overlap"
      ),
      in_cohort = is.na(.data$excluded_reason)
    ) |>
    dplyr::select(
      "formation_date", "cik", "n_prev", "n_curr", "n_both", "value_curr", "turnover",
      "excluded_reason", "in_cohort"
    ) |>
    dplyr::arrange(.data$formation_date, .data$cik)
}
