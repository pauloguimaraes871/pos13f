test_that("build_silver_filings keeps holdings filings only, with point-in-time dates", {
  filings <- silver_filings_fixture()

  ## The notice filing of CIK 0001002 is gone; the rows are ordered by filer
  expect_equal(filings$accession_number, c(
    "0000001001-22-000001", "0000001001-23-000001", "0000001003-23-000001"
  ))
  ## Monday filing usable Tuesday; filing on the first dollar-regime day usable the
  ## next day; Friday filing usable Monday
  expect_equal(filings$usable_date, as.Date(c("2022-11-15", "2023-01-04", "2023-02-13")))
})

test_that("build_silver_filings sets the unit regime by filing date", {
  filings <- silver_filings_fixture()
  ## The 2022Q3 restatement was filed on 2023-01-03, so it is in dollars
  expect_equal(filings$value_regime, c("thousands", "dollars", "dollars"))
  expect_equal(filings$period_of_report[2], as.Date("2022-09-30"))
})

test_that("build_silver_filings takes amendment status from the submission type", {
  filings <- silver_filings_fixture()
  expect_equal(filings$is_amendment, c(FALSE, TRUE, FALSE))
  ## The cover page of the last original says NEW HOLDINGS; it is not an amendment
  expect_equal(filings$amendment_type, c(NA, "RESTATEMENT", NA))
  expect_equal(filings$amendment_no, c(NA, 1, NA))
  expect_equal(filings$report_type, c("holdings", "holdings", "combination"))
})

test_that("build_silver_filings keeps a filing without a summary page", {
  filings <- silver_filings_fixture()
  expect_equal(filings$table_entry_total, c(120, 118, NA))
  expect_equal(filings$is_confidential_omitted, c(FALSE, TRUE, NA))
})

test_that("build_silver_filings stops on a holdings filing without a cover page", {
  coverpage <- dplyr::filter(silver_coverpage(), .data$ACCESSION_NUMBER != "0000001003-23-000001")
  expect_error(
    build_silver_filings(silver_submission(), coverpage, silver_summarypage(), silver_calendar)
  )
})

test_that("build_silver_filings names a missing column", {
  submission <- dplyr::select(silver_submission(), -"FILING_DATE")
  expect_error(
    build_silver_filings(submission, silver_coverpage(), silver_summarypage(), silver_calendar),
    "FILING_DATE"
  )
})
