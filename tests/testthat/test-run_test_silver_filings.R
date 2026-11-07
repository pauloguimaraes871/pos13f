silver_suite_passes <- function(filings, n_expected = 3, relations = NULL) {
  if (is.null(relations)) {
    relations <- build_silver_filing_relations(silver_othermanager(), silver_othermanager2(),
      filings = silver_filings_fixture()
    )
  }
  suppressMessages(run_logged_test(
    function() {
      run_test_silver_filings(filings, relations, silver_calendar, n_expected, silver_archives)
    },
    log_file = "test_silver_filings",
    log_path = withr::local_tempdir(.local_envir = parent.frame())
  ))
}

test_that("the silver filings suite passes on correct filings", {
  expect_true(silver_suite_passes(silver_filings_fixture()))
})

test_that("the suite fails when a filing is usable on its own filing date", {
  ## Look-ahead: a filing made after the close would be traded on the same day
  filings <- dplyr::mutate(silver_filings_fixture(), usable_date = .data$filing_date)
  expect_false(silver_suite_passes(filings))
})

## Replace the value of the first row only (the 2022Q3 original of CIK 0000001001)
set_first <- function(x, value) dplyr::if_else(seq_along(x) == 1, value, x)

test_that("the suite fails when usable_date skips a trading day", {
  filings <- dplyr::mutate(silver_filings_fixture(),
    usable_date = set_first(.data$usable_date, as.Date("2022-12-30"))
  )
  expect_false(silver_suite_passes(filings))
})

test_that("the suite fails when filing_date is outside the archive window", {
  ## A filing dated before the window of the archive SEC placed it in
  filings <- dplyr::mutate(silver_filings_fixture(),
    source_archive = set_first(.data$source_archive, "2023q1")
  )
  expect_false(silver_suite_passes(filings))
  ## An archive the sample does not list
  filings <- dplyr::mutate(silver_filings_fixture(),
    source_archive = set_first(.data$source_archive, "2021q1")
  )
  expect_false(silver_suite_passes(filings))
})

test_that("the suite fails when the accession year disagrees with filing_date", {
  filings <- dplyr::mutate(silver_filings_fixture(),
    accession_number = set_first(.data$accession_number, "0000001001-21-000001")
  )
  expect_false(silver_suite_passes(filings))
})

test_that("the suite accepts a filing on its quarter end but not before it", {
  ## The fixture calendar has no day between 2022-09-30 and 2022-11-14, so a 2022Q3
  ## report filed on the quarter end (archive 2022q3) is usable on 2022-11-14
  on_quarter_end <- dplyr::mutate(silver_filings_fixture(),
    filing_date = set_first(.data$filing_date, as.Date("2022-09-30")),
    usable_date = set_first(.data$usable_date, as.Date("2022-11-14")),
    source_archive = set_first(.data$source_archive, "2022q3")
  )
  expect_true(silver_suite_passes(on_quarter_end))
  before <- dplyr::mutate(silver_filings_fixture(),
    period_of_report = set_first(.data$period_of_report, as.Date("2022-12-31"))
  )
  expect_false(silver_suite_passes(before))
})

test_that("the suite fails when a bronze holdings filing was lost", {
  expect_false(silver_suite_passes(silver_filings_fixture(), n_expected = 4))
})

test_that("the suite fails on a unit regime not set by the filing date", {
  filings <- dplyr::mutate(silver_filings_fixture(), value_regime = "thousands")
  expect_false(silver_suite_passes(filings))
})

test_that("the suite fails on a relation of a filing not in silver", {
  relations <- tibble::tibble(
    accession_number = "0000001002-22-000001", relation = "reports_for_me",
    sequence_number = NA_real_, related_cik = "0001001", related_file_number = NA_character_,
    related_name = NA_character_
  )
  expect_false(silver_suite_passes(silver_filings_fixture(), relations = relations))
})

test_that("the suite passes on the toy sample of real filings", {
  ## Real noise: odd CIKs, missing summary pages, late reports, same-day amendments
  toy <- toy_bronze_sec()
  filings <- toy_silver_filings(toy)
  relations <- build_silver_filing_relations(toy$OTHERMANAGER, toy$OTHERMANAGER2, filings)
  n_expected <- sum(toy$SUBMISSION$SUBMISSIONTYPE %in% c("13F-HR", "13F-HR/A"))
  ok <- suppressMessages(run_logged_test(
    function() run_test_silver_filings(filings, relations, toy$calendar, n_expected, toy$archives),
    log_file = "test_silver_filings", log_path = withr::local_tempdir()
  ))
  expect_true(ok)
})

test_that("the suite fails on a weekend day in the trading calendar", {
  relations <- build_silver_filing_relations(silver_othermanager(), silver_othermanager2(),
    filings = silver_filings_fixture()
  )
  calendar <- sort(c(silver_calendar, as.Date("2022-11-19")))
  ok <- suppressMessages(run_logged_test(
    function() {
      run_test_silver_filings(silver_filings_fixture(), relations, calendar, 3, silver_archives)
    },
    log_file = "test_silver_filings", log_path = withr::local_tempdir()
  ))
  expect_false(ok)
})
