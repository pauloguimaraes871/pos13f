#' Data-quality suite for one bronze SEC archive
#'
#' Run by [deploy_bronze()] through [run_logged_test()] before the archive is pinned
#' (integration checks on real data; unit tests live in `tests/testthat/`). A failed
#' check raises an error and the archive is not pinned.
#'
#' @param tables Named list of six tibbles from [read_sec_archive()].
#' @param archive One-row data frame with `label`, `window_start` and `window_end` (Date)
#'   of the archive, from [list_sec_archives()].
#' @param min_filings,min_holdings Numeric scalars, minimum rows in SUBMISSION and
#'   INFOTABLE. Defaults are about two thirds of the smallest full archive measured on
#'   2026-11-07 (2013q4: 5,043 filings, 1,519,317 holdings), so a partial or truncated
#'   file fails. [deploy_bronze()] lowers them only for archives SEC publishes as partial.
#' @param max_missing_summary Numeric scalar, largest share of holdings reports
#'   (13F-HR, 13F-HR/A) allowed without a SUMMARYPAGE row. SEC itself misses a few: 18
#'   of about 400,000 over all 54 archives, at most 0.1% in one archive (measured
#'   2026-11-07), so the default 1% tolerates SEC's gaps but rejects a truncated table.
#'
#' @return `invisible(TRUE)` when every check passes.
#'
#' @details Checks, in order:
#' 1. Completeness: the six tables are present, SUBMISSION and INFOTABLE reach their
#'    minimum size, every source line became a row and readr reported no parsing
#'    problem.
#' 2. Schema: column names and classes equal `sec_table_schema()`.
#' 3. Keys: one row per primary key, as documented in the SEC readme; for
#'    OTHERMANAGER2, which has none, no exact duplicate row.
#' 4. Referential integrity: every child row belongs to a filing in SUBMISSION; every
#'    filing has a COVERPAGE row (true in all 54 archives); holdings reports have a
#'    SUMMARYPAGE row up to `max_missing_summary`.
#' 5. Content: required fields present, known submission types, non-negative amounts,
#'    filing dates inside the archive window. SEC moves filings made after 5:30 pm on
#'    the last business day into the next posting, so the window starts 5 days early.
#' @export
run_test_bronze_sec_archive <- function(tables, archive, min_filings = 3000,
                                        min_holdings = 1e6, max_missing_summary = 0.01) {
  schema <- sec_table_schema()
  ## OTHERMANAGER2 has no unique key: filers list several managers under one sequence
  ## number (verified in 2014q1, 2023q1, 2025), so its check is "no exact duplicate row"
  keys <- list(
    SUBMISSION = "ACCESSION_NUMBER", COVERPAGE = "ACCESSION_NUMBER",
    SUMMARYPAGE = "ACCESSION_NUMBER", OTHERMANAGER = c("ACCESSION_NUMBER", "OTHERMANAGER_SK"),
    OTHERMANAGER2 = names(schema$OTHERMANAGER2$cols),
    INFOTABLE = c("ACCESSION_NUMBER", "INFOTABLE_SK")
  )
  ## Class each readr collector produces, to compare with the columns read
  class_of <- c(
    collector_character = "character", collector_double = "numeric",
    collector_date = "Date"
  )

  # Completeness
  testthat::test_that(paste(archive$label, "has all tables and every source line"), {
    testthat::expect_named(tables, names(schema))
    testthat::expect_gte(nrow(tables$SUBMISSION), min_filings)
    testthat::expect_gte(nrow(tables$INFOTABLE), min_holdings)
    for (tbl in names(tables)) {
      testthat::expect_equal(nrow(tables[[tbl]]), attr(tables[[tbl]], "source_lines") - 1,
        label = paste(tbl, "rows")
      )
      testthat::expect_equal(attr(tables[[tbl]], "n_problems"), 0L,
        label = paste(tbl, "parsing problems")
      )
    }
  })

  # Schema
  testthat::test_that(paste(archive$label, "columns and classes match the schema"), {
    for (tbl in names(schema)) {
      expected <- unname(class_of[purrr::map_chr(schema[[tbl]]$cols, function(x) class(x)[1])])
      actual <- unname(purrr::map_chr(tables[[tbl]], function(x) class(x)[1]))
      actual[actual == "double"] <- "numeric"
      testthat::expect_equal(names(tables[[tbl]]), names(schema[[tbl]]$cols), label = tbl)
      testthat::expect_equal(actual, expected, label = paste(tbl, "classes"))
    }
  })

  # Keys and referential integrity
  testthat::test_that(paste(archive$label, "keys are unique and children have a filing"), {
    for (tbl in names(keys)) {
      n_keys <- nrow(dplyr::distinct(tables[[tbl]], dplyr::across(dplyr::all_of(keys[[tbl]]))))
      n_dup <- nrow(tables[[tbl]]) - n_keys
      testthat::expect_equal(n_dup, 0, label = paste(tbl, "duplicated keys"))
      orphans <- setdiff(tables[[tbl]]$ACCESSION_NUMBER, tables$SUBMISSION$ACCESSION_NUMBER)
      testthat::expect_length(orphans, 0)
    }
  })

  # Required children: a filing without its cover page loses amendment and report type
  testthat::test_that(paste(archive$label, "filings have their cover and summary pages"), {
    sub <- tables$SUBMISSION
    no_cover <- setdiff(sub$ACCESSION_NUMBER, tables$COVERPAGE$ACCESSION_NUMBER)
    testthat::expect_length(no_cover, 0)
    holdings_reports <- sub$ACCESSION_NUMBER[sub$SUBMISSIONTYPE %in% c("13F-HR", "13F-HR/A")]
    share_missing <- mean(!holdings_reports %in% tables$SUMMARYPAGE$ACCESSION_NUMBER)
    testthat::expect_lte(share_missing, max_missing_summary)
  })

  # Content
  testthat::test_that(paste(archive$label, "required fields, types, amounts and dates"), {
    sub <- tables$SUBMISSION
    required <- c("FILING_DATE", "PERIODOFREPORT", "CIK", "SUBMISSIONTYPE")
    known_types <- c("13F-HR", "13F-HR/A", "13F-NT", "13F-NT/A")
    in_window <- sub$FILING_DATE >= archive$window_start - 5 &
      sub$FILING_DATE <= archive$window_end
    testthat::expect_false(anyNA(sub[required]))
    testthat::expect_true(all(sub$SUBMISSIONTYPE %in% known_types))
    testthat::expect_true(all(in_window))
    info <- tables$INFOTABLE
    testthat::expect_false(anyNA(info[c("CUSIP", "VALUE", "SSHPRNAMT")]))
    testthat::expect_true(all(info$VALUE >= 0 & info$SSHPRNAMT >= 0))
  })

  invisible(TRUE)
}
