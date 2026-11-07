#' Deploy the silver filings layer: holdings filings, their relations and the calendar
#'
#' Reads the filing-level tables of every bronze SEC archive and the SPY dates of the
#' bronze Tiingo prices, builds `silver_filings` (one row per 13F holdings filing with
#' its `usable_date` and unit regime), `silver_filing_relations` and
#' `silver_trading_calendar`, runs [run_test_silver_filings()] and pins the three
#' tables only if every check passes.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param archives Data frame of the SEC archives in the sample window, from
#'   [list_sec_archives()] (filtered in `_targets.R`): `label`, `window_start`,
#'   `window_end`. Only these archives are read, and each filing is checked against its
#'   archive's filing window.
#' @param log_path Character scalar, folder for the data-quality log.
#' @param verbose Logical; print progress and pins messages.
#'
#' @return A tibble with one row per pinned table: `table` and `version`.
#'
#' @details
#' Lineage: the filing tables record the version of every bronze SEC pin read and of
#' `bronze_tiingo_prices` (the calendar behind `usable_date`); the calendar records the
#' Tiingo version only.
#' @export
deploy_silver_filings <- function(board, archives, log_path = file.path("logs", "tests"),
                                  verbose = TRUE) {
  # Validate inputs
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")
  required <- c("label", "window_start", "window_end")
  if (!is.data.frame(archives) || !all(required %in% names(archives)) || nrow(archives) == 0) {
    stop(
      "archives must be a non-empty data frame with columns: ",
      paste(required, collapse = ", "), " (see list_sec_archives())."
    )
  }

  # Read bronze
  read <- function(table) read_bronze_sec_table(board, table, labels = archives$label)
  submission <- read("SUBMISSION")
  calendar <- read_trading_calendar(board)
  if (verbose) message("Silver filings: read ", nrow(submission), " bronze submissions")

  # Build
  filings <- build_silver_filings(submission, read("COVERPAGE"), read("SUMMARYPAGE"),
    calendar = calendar$date
  )
  relations <- build_silver_filing_relations(read("OTHERMANAGER"), read("OTHERMANAGER2"),
    filings = filings
  )

  # Test, then pin
  n_expected <- sum(submission$SUBMISSIONTYPE %in% c("13F-HR", "13F-HR/A"))
  ok <- run_logged_test(
    function() {
      run_test_silver_filings(filings, relations, calendar$date, n_expected, archives)
    },
    log_file = "test_silver_filings", log_path = log_path
  )
  check_test_success(ok)
  inputs <- c(attr(submission, "inputs"), attr(calendar, "inputs"))
  pin <- function(data, table, inputs) {
    save_local_pin(board, data, "silver", table, inputs = inputs, verbose = verbose)
  }
  tibble::tibble(
    table = c("filings", "filing_relations", "trading_calendar"),
    version = c(
      pin(filings, "filings", inputs),
      pin(relations, "filing_relations", inputs),
      pin(tibble::tibble(date = calendar$date), "trading_calendar", attr(calendar, "inputs"))
    )
  )
}
