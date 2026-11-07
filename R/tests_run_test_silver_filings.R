#' Data-quality suite for the silver filings and their relations
#'
#' Run by [deploy_silver_filings()] before anything is pinned. Silver is the strict
#' layer: every check here must hold on every row, without tolerance, because the
#' point-in-time contract of the study rests on `usable_date`.
#'
#' @param filings Tibble from `build_silver_filings()`.
#' @param relations Tibble from `build_silver_filing_relations()`.
#' @param calendar Date vector, trading days (`read_trading_calendar()`).
#' @param n_expected Numeric scalar, number of 13F-HR and 13F-HR/A submissions in
#'   bronze; silver must keep every one of them.
#' @param archives Data frame of the SEC archives read, from [list_sec_archives()]:
#'   `label`, `window_start` and `window_end` (Date, the filing-date window of the
#'   archive).
#' @param max_quarter_change Numeric scalar, largest relative change allowed in the
#'   number of original reports from one report quarter to the next.
#' @param min_quarter_filings Numeric scalar, the continuity check only compares
#'   quarters with at least this many original reports (small samples are noisy).
#'
#' @return `invisible(TRUE)` when every check passes.
#'
#' @details
#' The `usable_date` checks recompute the rule independently of
#' `compute_usable_date()`: the date must be a trading day, later than the filing date,
#' with no trading day in between.
#'
#' `filing_date` itself is confirmed by two independent sources: SEC placed the filing
#' in the archive whose filing window contains that date, and the accession number
#' (`<submitter CIK>-<YY>-<sequence>`) carries the same year.
#'
#' Coverage continuity catches a missing or truncated archive, which no row-level check
#' sees: original reports per quarter grew smoothly from 3,453 (2013Q2) to 8,908
#' (2025Q4), with the largest step +14% (Q4 2021, year-end registrations). Only
#' quarters whose filing deadline (quarter end plus 45 days) lies inside the sample's
#' filing dates are compared.
#' @export
run_test_silver_filings <- function(filings, relations, calendar, n_expected, archives,
                                    max_quarter_change = 0.25, min_quarter_filings = 1000) {
  testthat::test_that("the trading calendar is increasing weekdays with full years", {
    testthat::expect_false(is.unsorted(calendar, strictly = TRUE))
    testthat::expect_false(any(lubridate::wday(calendar, week_start = 1) > 5))
    ## A full year has 250 to 253 NYSE sessions; fewer means missing SPY days
    years <- table(lubridate::year(calendar))
    full_years <- names(years)[
      as.Date(paste0(names(years), "-01-07")) >= min(calendar) &
        as.Date(paste0(names(years), "-12-24")) <= max(calendar)
    ]
    testthat::expect_true(all(years[full_years] >= 248 & years[full_years] <= 254))
  })

  testthat::test_that("original reports per quarter change smoothly (no lost archive)", {
    per_quarter <- filings |>
      dplyr::filter(
        !.data$is_amendment,
        .data$period_of_report + 45 >= min(.data$filing_date),
        .data$period_of_report + 45 <= max(.data$filing_date)
      ) |>
      dplyr::count(.data$period_of_report) |>
      dplyr::filter(.data$n >= min_quarter_filings) |>
      dplyr::mutate(
        change = .data$n / dplyr::lag(.data$n) - 1,
        ## A quarter with no report at all would vanish from the count: check the step
        quarter_index = lubridate::year(.data$period_of_report) * 4 +
          lubridate::quarter(.data$period_of_report),
        step = .data$quarter_index - dplyr::lag(.data$quarter_index)
      )
    testthat::expect_true(all(abs(per_quarter$change) <= max_quarter_change, na.rm = TRUE))
    testthat::expect_true(all(per_quarter$step == 1, na.rm = TRUE))
  })

  testthat::test_that("every bronze holdings filing appears exactly once", {
    testthat::expect_equal(nrow(filings), n_expected)
    testthat::expect_false(anyDuplicated(filings$accession_number) > 0)
    testthat::expect_true(all(filings$submission_type %in% c("13F-HR", "13F-HR/A")))
    testthat::expect_false(anyNA(filings$cik))
    testthat::expect_true(all(stringr::str_detect(filings$cik, "^[0-9]{1,10}$")))
  })

  testthat::test_that("usable_date is the first trading day after the filing date", {
    testthat::expect_false(anyNA(filings$usable_date))
    testthat::expect_true(all(filings$usable_date > filings$filing_date))
    testthat::expect_true(all(filings$usable_date %in% calendar))
    ## No trading day strictly between the filing date and the usable date
    days_before_usable <- findInterval(as.numeric(filings$usable_date) - 1, as.numeric(calendar))
    days_by_filing <- findInterval(as.numeric(filings$filing_date), as.numeric(calendar))
    testthat::expect_equal(days_before_usable, days_by_filing)
  })

  testthat::test_that("filing_date agrees with the archive window and the accession year", {
    windows <- dplyr::select(archives, source_archive = "label", "window_start", "window_end")
    placed <- dplyr::left_join(filings, windows,
      by = dplyr::join_by("source_archive"), relationship = "many-to-one"
    )
    testthat::expect_false(anyNA(placed$window_start))
    testthat::expect_true(all(
      placed$filing_date >= placed$window_start & placed$filing_date <= placed$window_end
    ))
    accession_year <- stringr::str_match(
      filings$accession_number, "^[0-9]{10}-([0-9]{2})-[0-9]{6}$"
    )[, 2]
    testthat::expect_equal(accession_year, format(filings$filing_date, "%y"))
  })

  testthat::test_that("each filing reports a quarter that had ended when it was filed", {
    period <- filings$period_of_report
    testthat::expect_false(anyNA(period))
    testthat::expect_true(all(lubridate::month(period) %in% c(3, 6, 9, 12)))
    testthat::expect_true(all(period == lubridate::ceiling_date(period, "month") - 1))
    ## Five filings (2016 to 2026) are dated on the quarter end itself; usable_date still
    ## falls after it, so no position is used before the quarter is over
    testthat::expect_true(all(filings$filing_date >= period))
  })

  testthat::test_that("units and amendment fields follow their rules", {
    regime <- dplyr::if_else(filings$filing_date < as.Date("2023-01-03"), "thousands", "dollars")
    testthat::expect_equal(filings$value_regime, regime)
    testthat::expect_equal(filings$is_amendment, filings$submission_type == "13F-HR/A")
    testthat::expect_true(all(is.na(filings$amendment_type[!filings$is_amendment])))
    testthat::expect_true(all(
      filings$amendment_type %in% c("RESTATEMENT", "NEW HOLDINGS", NA_character_)
    ))
    testthat::expect_true(all(filings$report_type %in% c("holdings", "combination")))
  })

  testthat::test_that("relations belong to silver filings and name a manager", {
    testthat::expect_true(all(relations$accession_number %in% filings$accession_number))
    testthat::expect_true(all(relations$relation %in% c("reports_for_me", "included_in_report")))
    identified <- !is.na(relations$related_cik) | !is.na(relations$related_file_number) |
      !is.na(relations$related_name)
    testthat::expect_true(all(identified))
  })

  invisible(TRUE)
}
