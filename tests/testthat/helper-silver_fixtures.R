# Fixtures for the silver filings layer ----
#
# Three filers, ordered by CIK, with an unbalanced history:
#   0001001  an original 13F-HR for 2022Q3, then a restatement filed on 2023-01-03 (the
#            first day of the dollar regime), Friday filing included
#   0001002  a notice filing only (13F-NT): no holdings, must be dropped
#   0001003  a combination report whose cover page wrongly flags it as an amendment,
#            filed on a Friday, without a summary page

## Trading days around the filings; 2022-12-31 to 2023-01-02 is a weekend plus holiday
silver_calendar <- as.Date(c(
  "2022-11-14", "2022-11-15", "2022-12-30", "2023-01-03", "2023-01-04", "2023-02-10",
  "2023-02-13", "2023-02-14"
))

## Filing-date windows of the archives, as list_sec_archives() returns them
silver_archives <- tibble::tibble(
  label = c("2022q3", "2022q4", "2023q1"),
  window_start = as.Date(c("2022-07-01", "2022-10-01", "2023-01-01")),
  window_end = as.Date(c("2022-09-30", "2022-12-31", "2023-03-31"))
)

silver_submission <- function() {
  tibble::tibble(
    ACCESSION_NUMBER = c(
      "0000001001-22-000001", "0000001001-23-000001", "0000001002-22-000001",
      "0000001003-23-000001"
    ),
    FILING_DATE = as.Date(c("2022-11-14", "2023-01-03", "2022-11-14", "2023-02-10")),
    SUBMISSIONTYPE = c("13F-HR", "13F-HR/A", "13F-NT", "13F-HR"),
    CIK = c("0001001", "0001001", "0001002", "0001003"),
    PERIODOFREPORT = as.Date(c("2022-09-30", "2022-09-30", "2022-09-30", "2022-12-31")),
    source_archive = c("2022q4", "2023q1", "2022q4", "2023q1")
  )
}

silver_coverpage <- function() {
  tibble::tibble(
    ACCESSION_NUMBER = silver_submission()$ACCESSION_NUMBER,
    REPORTTYPE = c(
      "13F HOLDINGS REPORT", "13F HOLDINGS REPORT", "13F NOTICE", "13F COMBINATION REPORT"
    ),
    ISAMENDMENT = c("N", "Y", "N", "Y"),
    AMENDMENTTYPE = c(NA, "RESTATEMENT", NA, "NEW HOLDINGS"),
    AMENDMENTNO = c(NA, 1, NA, 1),
    FILINGMANAGER_NAME = c("Alpha Capital", "Alpha Capital", "Beta Advisers", "Gamma LP"),
    FORM13FFILENUMBER = c("028-10001", "028-10001", "028-10002", "028-10003")
  )
}

silver_summarypage <- function() {
  tibble::tibble(
    ACCESSION_NUMBER = c("0000001001-22-000001", "0000001001-23-000001", "0000001002-22-000001"),
    OTHERINCLUDEDMANAGERSCOUNT = c(0, 1, 0),
    TABLEENTRYTOTAL = c(120, 118, 0),
    TABLEVALUETOTAL = c(450000, 452000000, 0),
    ISCONFIDENTIALOMITTED = c("N", "Y", "N")
  )
}

silver_othermanager <- function() {
  ## The notice filer names the manager that reports for it; that relation is dropped
  tibble::tibble(
    ACCESSION_NUMBER = c("0000001001-22-000001", "0000001002-22-000001"),
    OTHERMANAGER_SK = c(11, 12),
    CIK = c(NA, "0001001"),
    FORM13FFILENUMBER = c("028-20001", "028-10001"),
    CRDNUMBER = NA_character_, SECFILENUMBER = NA_character_,
    NAME = c("Delta Trust", "Alpha Capital")
  )
}

silver_othermanager2 <- function() {
  ## The restatement lists one included manager, twice at the same position
  tibble::tibble(
    ACCESSION_NUMBER = c("0000001001-23-000001", "0000001001-23-000001"),
    SEQUENCENUMBER = c(1, 1),
    CIK = c("0001004", "0001004"),
    FORM13FFILENUMBER = c("028-10004", "028-10004"),
    CRDNUMBER = NA_character_, SECFILENUMBER = NA_character_,
    NAME = c("Epsilon Partners", "Epsilon Partners")
  )
}

silver_filings_fixture <- function() {
  build_silver_filings(silver_submission(), silver_coverpage(), silver_summarypage(),
    calendar = silver_calendar
  )
}

## Board with two bronze SEC archives (filing tables only) and bronze Tiingo prices
setup_silver_board <- function(env = parent.frame()) {
  board <- init_pipeline_board(withr::local_tempdir(.local_envir = env))
  dir <- withr::local_tempdir(.local_envir = env)
  tables <- list(
    SUBMISSION = silver_submission(), COVERPAGE = silver_coverpage(),
    SUMMARYPAGE = silver_summarypage(), OTHERMANAGER = silver_othermanager(),
    OTHERMANAGER2 = silver_othermanager2()
  )
  for (label in c("2022q4", "2023q1")) {
    rows <- silver_submission()$ACCESSION_NUMBER[silver_submission()$source_archive == label]
    files <- purrr::imap_chr(tables, function(df, name) {
      df <- dplyr::filter(df, .data$ACCESSION_NUMBER %in% rows)
      if (name == "SUBMISSION") df <- dplyr::select(df, -"source_archive")
      path <- file.path(dir, label, paste0(name, ".parquet"))
      dir.create(dirname(path), showWarnings = FALSE)
      nanoparquet::write_parquet(df, path)
      path
    })
    save_local_pin(board, unname(files), "bronze", paste0("sec_", label), verbose = FALSE)
  }
  ## SPY trades on the calendar days; another ticker on fewer days, and a 404 row
  prices <- dplyr::bind_rows(
    tibble::tibble(ticker = "SPY", status = "200", date = silver_calendar, close = 400),
    tibble::tibble(ticker = "AAPL", status = "200", date = silver_calendar[1:3], close = 150),
    tibble::tibble(ticker = "ZZZZ", status = "404", date = as.Date(NA), close = NA_real_)
  )
  price_path <- file.path(dir, "TIINGO_PRICES.parquet")
  nanoparquet::write_parquet(prices, price_path)
  save_local_pin(board, price_path, "bronze", "tiingo_prices", verbose = FALSE)
  board
}
