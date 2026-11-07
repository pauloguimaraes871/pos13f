# Scenario fixtures for the silver holdings state machine ----
#
# Three filers, ordered by CIK and period, built to hit every case of plan Section 4:
#   0000000101 2022Q3  original, NEW HOLDINGS, RESTATEMENT filed in the dollar regime,
#                      then NEW HOLDINGS again
#   0000000101 2022Q4  original, then an amendment without a type (ignored)
#   0000000202 2022Q4  NEW HOLDINGS filed before its original, which then replaces it
#   0000000202 2023Q1  original and RESTATEMENT usable on the same day
#   0000000303 2022Q2  original filed in February 2023: lands in the 2023q1 archive,
#                      still reports 2022Q2, usable only from its own date
#   0000000303 2022Q3  original filed on time
holdings_filings_fixture <- function() {
  ## One element per filing, in the order of the table above
  filer <- rep(c("0000000101", "0000000202", "0000000303"), times = c(6, 4, 2))
  number <- c(
    "22-000001", "22-000002", "23-000001", "23-000002", "23-000003", "23-000004",
    "23-000001", "23-000002", "23-000003", "23-000004", "22-000001", "23-000001"
  )
  amendment <- c(
    "", "NEW HOLDINGS", "RESTATEMENT", "NEW HOLDINGS", "", "untyped",
    "NEW HOLDINGS", "", "", "RESTATEMENT", "", ""
  )
  filing_date <- as.Date(c(
    "2022-11-14", "2022-12-01", "2023-02-01", "2023-02-10", "2023-02-14", "2023-03-01",
    "2023-02-01", "2023-02-14", "2023-05-15", "2023-05-15", "2022-11-14", "2023-02-09"
  ))
  tibble::tibble(
    accession_number = paste0(filer, "-", number),
    cik = filer,
    period_of_report = as.Date(c(
      rep("2022-09-30", 4), rep("2022-12-31", 4), rep("2023-03-31", 2), "2022-09-30",
      "2022-06-30"
    )),
    filing_date = filing_date,
    usable_date = as.Date(c(
      "2022-11-15", "2022-12-02", "2023-02-02", "2023-02-13", "2023-02-15", "2023-03-02",
      "2023-02-02", "2023-02-15", "2023-05-16", "2023-05-16", "2022-11-15", "2023-02-10"
    )),
    is_amendment = amendment != "",
    amendment_type = dplyr::case_when(amendment %in% c("", "untyped") ~ NA, .default = amendment),
    value_regime = dplyr::if_else(filing_date < as.Date("2023-01-03"), "thousands", "dollars"),
    source_archive = paste0(lubridate::year(filing_date), "q", lubridate::quarter(filing_date)),
    table_entry_total = c(5, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1)
  )
}

## The rows of one filer and report quarter
quarter_of <- function(df, filer, period) {
  dplyr::filter(df, .data$cik == .env$filer, .data$period_of_report == as.Date(.env$period))
}

## Bronze INFOTABLE rows of the fixture filings (VALUE in each filing's unit). AAPL
## 037833100, MSFT 594918104 and TSLA 88160R101 are real CUSIPs with valid check digits
holdings_infotable_fixture <- function() {
  tibble::tribble(
    ~ACCESSION_NUMBER, ~CUSIP, ~VALUE, ~SSHPRNAMT, ~SSHPRNAMTTYPE, ~PUTCALL,
    ## Original 2022Q3: AAPL on two rows (discretion split, summed), MSFT, plus an option
    ## and a bond that are not share positions
    "0000000101-22-000001", "037833100", 100, 600, "SH", NA,
    "0000000101-22-000001", "037833100", 50, 300, "SH", NA,
    "0000000101-22-000001", "594918104", 200, 800, "SH", NA,
    "0000000101-22-000001", "037833100", 10, 100, "SH", "Call",
    "0000000101-22-000001", "88160R101", 70, 70000, "PRN", NA,
    "0000000101-22-000002", "88160R101", 30, 100, "SH", NA,
    ## Restatement in dollars: AAPL only; then NEW HOLDINGS adds MSFT
    "0000000101-23-000001", "037833100", 140000, 900, "SH", NA,
    "0000000101-23-000002", "594918104", 190000, 800, "SH", NA,
    ## 2022Q4: lower-case CUSIP with spaces is the same AAPL; a malformed one is dropped
    "0000000101-23-000003", " 037833100", 150000, 1000, "SH", NA,
    "0000000101-23-000003", "037833101", 5000, 10, "SH", NA,
    "0000000101-23-000004", "594918104", 1, 1, "SH", NA,
    "0000000202-23-000001", "88160R101", 40000, 200, "SH", NA,
    "0000000202-23-000002", "594918104", 60000, 250, "SH", NA,
    "0000000202-23-000003", "594918104", 61000, 250, "SH", NA,
    "0000000202-23-000004", "594918104", 62000, 260, "SH", NA,
    "0000000303-22-000001", "037833100", 20, 140, "SH", NA,
    "0000000303-23-000001", "037833100", 25000, 180, "SH", NA
  )
}

## The suite figures of the fixture, for a given (possibly damaged) state
checks_fixture <- function(state = holdings_state_fixture()) {
  filings <- holdings_filings_fixture()
  infotable <- holdings_infotable_fixture()
  positions <- summarise_holdings_positions(infotable, list_valid_position_cusips(infotable),
    regimes = dplyr::select(filings, "accession_number", "value_regime")
  )
  sources <- build_holdings_sources(order_holdings_events(filings))
  summarise_holdings_checks(state, positions, sources, infotable)
}

## The holdings state of the fixture, built locally (same code as in duckdb)
holdings_state_fixture <- function() {
  filings <- holdings_filings_fixture()
  infotable <- holdings_infotable_fixture()
  positions <- summarise_holdings_positions(infotable,
    valid_cusips = list_valid_position_cusips(infotable),
    regimes = dplyr::select(filings, "accession_number", "value_regime")
  )
  sources <- build_holdings_sources(order_holdings_events(filings))
  build_silver_holdings_state(positions, sources) |>
    dplyr::arrange(.data$cik, .data$period_of_report, .data$valid_from, .data$cusip)
}
