mini_tables <- function() {
  zip_path <- withr::local_tempfile(fileext = "_form13f.zip", .local_envir = parent.frame())
  zip::zip(zip_path,
    files = list.files(test_path("fixtures", "sec_mini"), full.names = TRUE),
    mode = "cherry-pick"
  )
  read_sec_archive(zip_path, verbose = FALSE)
}
mini_archive <- tibble::tibble(
  file = "2014q1_form13f.zip", label = "2014q1",
  window_start = as.Date("2014-01-01"), window_end = as.Date("2014-03-31")
)

# Run the suite as a deploy does: through the stop reporter, TRUE or FALSE. The mini
# archive has 3 filings and 7 holdings, so the size floors are lowered to fit it.
suite_passes <- function(tables, archive = mini_archive, min_filings = 1, min_holdings = 1) {
  suppressMessages(run_logged_test(
    function() {
      run_test_bronze_sec_archive(tables, archive,
        min_filings = min_filings,
        min_holdings = min_holdings
      )
    },
    log_file = "test_bronze", log_path = withr::local_tempdir(.local_envir = parent.frame())
  ))
}

test_that("the bronze suite passes on a well-formed archive", {
  expect_true(suite_passes(mini_tables()))
})

test_that("the bronze suite fails on an archive too small to be complete", {
  ## A complete archive with fewer rows than the floor is a partial or truncated file
  expect_false(suite_passes(mini_tables(), min_filings = 4))
  expect_false(suite_passes(mini_tables(), min_holdings = 8))
})

test_that("the bronze suite's default floors reject the mini archive", {
  ## Defaults are sized for real archives (smallest full archive: 5,043 filings,
  ## 1,519,317 holdings), so a 3-filing archive must fail them
  ok <- suppressMessages(run_logged_test(
    function() run_test_bronze_sec_archive(mini_tables(), mini_archive),
    log_file = "test_bronze", log_path = withr::local_tempdir()
  ))
  expect_false(ok)
})

test_that("the bronze suite fails when rows were lost while reading", {
  tables <- mini_tables()
  attr(tables$INFOTABLE, "source_lines") <- 99
  expect_false(suite_passes(tables))
})

test_that("the bronze suite fails on a duplicated filing key", {
  tables <- mini_tables()
  dup <- dplyr::bind_rows(tables$SUBMISSION, tables$SUBMISSION[1, ])
  tables$SUBMISSION <- structure(dup, source_lines = nrow(dup) + 1, n_problems = 0L)
  expect_false(suite_passes(tables))
})

test_that("OTHERMANAGER2 may repeat a sequence number but not a whole row", {
  ## Real filers list several managers under one sequence number (seen in 2014q1,
  ## 2023q1 and 2025): distinct rows sharing a key are valid
  tables <- mini_tables()
  om2 <- tables$OTHERMANAGER2
  shared <- dplyr::bind_rows(om2, dplyr::mutate(om2, NAME = "OTHER MANAGER LLC", CIK = NA))
  tables$OTHERMANAGER2 <- structure(shared, source_lines = 3, n_problems = 0L)
  expect_true(suite_passes(tables))

  exact <- dplyr::bind_rows(om2, om2)
  tables$OTHERMANAGER2 <- structure(exact, source_lines = 3, n_problems = 0L)
  expect_false(suite_passes(tables))
})

test_that("the bronze suite fails when a filing has no cover page (Codex B01)", {
  ## Cover pages carry amendment type, report type and confidential flags; all 54 real
  ## archives have one for every filing
  tables <- mini_tables()
  cov <- tables$COVERPAGE[-1, ]
  tables$COVERPAGE <- structure(cov, source_lines = nrow(cov) + 1, n_problems = 0L)
  expect_false(suite_passes(tables))
})

test_that("the bronze suite fails when too many holdings reports lack a summary page", {
  ## Real archives miss at most 0.1% (18 of about 400,000 reports in total), so the
  ## default tolerance is 1%; the mini archive has 2 holdings reports
  tables <- mini_tables()
  smry <- tables$SUMMARYPAGE[-1, ]
  tables$SUMMARYPAGE <- structure(smry, source_lines = nrow(smry) + 1, n_problems = 0L)
  expect_false(suite_passes(tables))
  ok <- suppressMessages(run_logged_test(
    function() {
      run_test_bronze_sec_archive(tables, mini_archive,
        min_filings = 1, min_holdings = 1, max_missing_summary = 0.6
      )
    },
    log_file = "test_bronze", log_path = withr::local_tempdir()
  ))
  expect_true(ok)
})

test_that("the bronze suite fails on a holding whose filing is unknown", {
  tables <- mini_tables()
  tables$INFOTABLE$ACCESSION_NUMBER[1] <- "0000000000-00-000000"
  expect_false(suite_passes(tables))
})

test_that("the bronze suite fails when a filing date falls outside the archive window", {
  tables <- mini_tables()
  tables$SUBMISSION$FILING_DATE[1] <- as.Date("2015-06-30")
  expect_false(suite_passes(tables))
})

test_that("the bronze suite fails when a column has the wrong class", {
  tables <- mini_tables()
  tables$INFOTABLE$VALUE <- as.character(tables$INFOTABLE$VALUE)
  expect_false(suite_passes(tables))
})
