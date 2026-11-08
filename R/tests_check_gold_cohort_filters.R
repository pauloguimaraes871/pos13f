#' Gold suite block: the cohort screens did what the plan says
#'
#' Called by [run_test_gold()]. Re-reads the filings, independently of the holdings
#' version table the cohort was built from, to confirm that every cohort filer had both
#' reports public at the formation date; checks that every filer with both report
#' versions current at t is in the cohort table; and recomputes the few-positions,
#' index-like and overlap exclusions.
#'
#' @param cohort Tibble from `select_gold_cohort()`.
#' @param filings Tibble pinned as `silver_filings` (`accession_number`, `cik`,
#'   `period_of_report`, `usable_date`).
#' @param versions Tibble pinned as `silver_holdings_versions`.
#' @param relations Tibble pinned as `silver_filing_relations`.
#' @param formations Tibble from `build_formation_dates()`.
#' @param min_positions Numeric scalar, fewest positions in each quarter (plan 1.9: 10).
#'
#' @return `NULL`, invisibly; called for its `testthat` expectations.
#' @noRd
check_gold_cohort_filters <- function(cohort, filings, versions, relations, formations,
                                      min_positions) {
  # Report versions current at t, per formation and compared quarter
  quarters <- dplyr::bind_rows(
    dplyr::transmute(formations, .data$formation_date, period_of_report = .data$report_quarter),
    dplyr::transmute(formations, .data$formation_date, period_of_report = .data$prev_quarter)
  )
  current <- versions |>
    dplyr::filter(.data$is_visible) |>
    dplyr::inner_join(quarters,
      by = dplyr::join_by("period_of_report"), relationship = "many-to-many"
    ) |>
    dplyr::filter(
      .data$valid_from <= .data$formation_date,
      is.na(.data$valid_to) | .data$valid_to > .data$formation_date
    )

  testthat::test_that("every cohort filer had both reports public at the formation date", {
    public <- function(period_col) {
      filings |>
        dplyr::inner_join(
          dplyr::select(formations, "formation_date", period_of_report = dplyr::all_of(period_col)),
          by = dplyr::join_by("period_of_report"), relationship = "many-to-many"
        ) |>
        dplyr::filter(.data$usable_date <= .data$formation_date) |>
        dplyr::distinct(.data$formation_date, .data$cik)
    }
    both <- dplyr::inner_join(public("prev_quarter"), public("report_quarter"),
      by = dplyr::join_by("formation_date", "cik"), relationship = "one-to-one"
    )
    unsupported <- dplyr::anti_join(cohort, both, by = dplyr::join_by("formation_date", "cik"))
    testthat::expect_equal(nrow(unsupported), 0)
  })

  testthat::test_that("every filer with both reports current at t is in the cohort table", {
    eligible <- current |>
      dplyr::distinct(.data$formation_date, .data$cik, .data$period_of_report) |>
      dplyr::count(.data$formation_date, .data$cik) |>
      dplyr::filter(.data$n == 2)
    key <- dplyr::join_by("formation_date", "cik")
    testthat::expect_equal(nrow(dplyr::anti_join(eligible, cohort, by = key)), 0)
    testthat::expect_equal(nrow(dplyr::anti_join(cohort, eligible, by = key)), 0)
  })

  testthat::test_that("few-positions and index-like exclusions match a recomputation", {
    expected <- cohort |>
      dplyr::mutate(
        few = pmin(.data$n_prev, .data$n_curr) < min_positions,
        positions_cut = stats::quantile(.data$n_curr[!.data$few], 2 / 3, names = FALSE),
        turnover_cut = stats::quantile(.data$turnover[!.data$few], 1 / 3, names = FALSE),
        index_like = !.data$few & .data$n_curr >= .data$positions_cut &
          .data$turnover <= .data$turnover_cut,
        .by = "formation_date"
      )
    testthat::expect_identical(expected$few, expected$excluded_reason %in% "few_positions")
    testthat::expect_identical(expected$index_like, expected$excluded_reason %in% "index_like")
    members <- dplyr::filter(cohort, .data$in_cohort)
    testthat::expect_true(all(pmin(members$n_prev, members$n_curr) >= min_positions))
    testthat::expect_identical(cohort$in_cohort, is.na(cohort$excluded_reason))
  })

  testthat::test_that("every overlap exclusion has an inclusion link in a report current at t", {
    strip <- function(x) sub("^0+", "", trimws(x))
    candidates <- cohort |>
      dplyr::filter(!.data$excluded_reason %in% c("few_positions", "index_like")) |>
      dplyr::transmute(.data$formation_date, key = strip(.data$cik))
    ## A current version is its base filing plus the NEW HOLDINGS amendments after it;
    ## a superseded filing's links no longer belong to the report
    contributing <- current |>
      dplyr::select("formation_date", "cik", "period_of_report", "event_no", "base_no") |>
      dplyr::inner_join(
        dplyr::select(versions, "cik", "period_of_report",
          event = "event_no", "kind",
          "accession_number"
        ),
        by = dplyr::join_by("cik", "period_of_report", x$base_no <= y$event, x$event_no >= y$event),
        relationship = "many-to-many"
      ) |>
      dplyr::filter(.data$event == .data$base_no | .data$kind == "append")
    links <- relations |>
      dplyr::filter(.data$relation == "included_in_report", !is.na(.data$related_cik)) |>
      dplyr::inner_join(dplyr::select(contributing, "formation_date", "cik", "accession_number"),
        by = dplyr::join_by("accession_number"), relationship = "many-to-many"
      ) |>
      dplyr::transmute(.data$formation_date, a = strip(.data$cik), b = strip(.data$related_cik)) |>
      dplyr::filter(.data$a != .data$b) |>
      dplyr::semi_join(dplyr::rename(candidates, a = "key"),
        by = dplyr::join_by("formation_date", "a")
      ) |>
      dplyr::semi_join(dplyr::rename(candidates, b = "key"),
        by = dplyr::join_by("formation_date", "b")
      )
    linked <- dplyr::bind_rows(
      dplyr::select(links, "formation_date", key = "a"),
      dplyr::select(links, "formation_date", key = "b")
    )
    dropped <- cohort |>
      dplyr::filter(.data$excluded_reason %in% "overlap") |>
      dplyr::transmute(.data$formation_date, key = strip(.data$cik))
    unlinked <- dplyr::anti_join(dropped, linked, by = dplyr::join_by("formation_date", "key"))
    testthat::expect_equal(nrow(unlinked), 0)
  })
  invisible(NULL)
}
