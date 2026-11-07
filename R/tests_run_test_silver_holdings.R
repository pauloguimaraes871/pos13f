#' Data-quality suite for the silver holdings state
#'
#' Run by `deploy_silver_holdings()` before anything is pinned. The point-in-time
#' checks run on the version tables (small, in R); the row checks on aggregates that
#' duckdb computes over the full state (`summarise_holdings_checks()`).
#'
#' @param events Tibble from `order_holdings_events()`.
#' @param sources Tibble from `build_holdings_sources()`.
#' @param filings Tibble of silver filings (`accession_number`, `cik`,
#'   `period_of_report`, `usable_date`, `is_amendment`, `amendment_type`,
#'   `table_entry_total`).
#' @param checks List from `summarise_holdings_checks()`.
#' @param max_entry_mismatch Numeric scalar, largest share of filings whose bronze row
#'   count differs by more than 5% from the entry total on their summary page.
#'
#' @return `invisible(TRUE)` when every check passes.
#'
#' @details
#' The row-count reconciliation tolerates filer errors on the summary page (1.25% of
#' filings differ by more than 5% on 2026-11-07); a parsing loss would push it up for a
#' whole archive.
#' @export
run_test_silver_holdings <- function(events, sources, filings, checks,
                                     max_entry_mismatch = 0.02) {
  dated <- dplyr::inner_join(sources,
    dplyr::select(filings, "accession_number", "usable_date", source_period = "period_of_report"),
    by = dplyr::join_by("accession_number"), relationship = "many-to-one"
  )

  testthat::test_that("a version starts exactly when its last filing becomes usable", {
    ## Look-ahead guard: no position is known before the filing reporting it is public
    testthat::expect_equal(nrow(dated), nrow(sources))
    testthat::expect_true(all(dated$usable_date <= dated$valid_from))
    starts <- dplyr::summarise(dated,
      last_usable = max(.data$usable_date),
      .by = c("cik", "period_of_report", "valid_from")
    )
    testthat::expect_equal(starts$last_usable, starts$valid_from)
  })

  testthat::test_that("the quarter comes from period_of_report, never from the archive", {
    testthat::expect_equal(dated$source_period, dated$period_of_report)
  })

  testthat::test_that("versions of a quarter follow each other without gap or overlap", {
    visible <- dplyr::filter(events, .data$is_visible) |>
      dplyr::mutate(next_from = dplyr::lead(.data$valid_from), .by = c("cik", "period_of_report"))
    testthat::expect_equal(visible$valid_to, visible$next_from)
  })

  testthat::test_that("every applied filing reaches the state unless replaced that day", {
    unused <- dplyr::anti_join(events, sources, by = dplyr::join_by("accession_number"))
    testthat::expect_false(any(unused$is_visible))
    n_untyped <- sum(filings$is_amendment & is.na(filings$amendment_type))
    testthat::expect_equal(attr(events, "n_untyped_ignored"), n_untyped)
    testthat::expect_equal(nrow(events), nrow(filings) - n_untyped)
  })

  testthat::test_that("state rows are unique, complete, non-negative and conserved", {
    totals <- checks$totals
    testthat::expect_equal(totals$n_duplicate_keys, 0)
    testthat::expect_equal(totals$n_negative, 0)
    testthat::expect_equal(totals$n_missing, 0)
    testthat::expect_equal(totals$shares_single_source_state,
      totals$shares_single_source_positions,
      tolerance = 1e-9
    )
  })

  testthat::test_that("bronze row counts match the summary page for nearly all filings", {
    compared <- filings |>
      dplyr::filter(.data$table_entry_total > 0) |>
      dplyr::inner_join(checks$entry_counts,
        by = dplyr::join_by("accession_number"), relationship = "one-to-one"
      )
    off <- abs(compared$n_infotable_rows - compared$table_entry_total) >
      0.05 * compared$table_entry_total
    testthat::expect_lte(mean(off), max_entry_mismatch)
  })

  invisible(TRUE)
}
