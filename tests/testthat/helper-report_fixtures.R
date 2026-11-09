# Fixtures for the report tables ----
#
# Small, uneven samples shaped like the pinned tables: two report quarters of filings
# with late reports and amendments, a two-formation universe, and a two-ladder backtest.
report_filings_fixture <- function() {
  tibble::tibble(
    cik = c("1", "2", "3", "1", "2", "1"),
    period_of_report = as.Date(c(
      "2019-03-31", "2019-03-31", "2019-03-31", "2019-06-30", "2019-06-30", "2019-06-30"
    )),
    ## Amendments carry their own form type, as in the SEC data sets
    submission_type = c("13F-HR", "13F-HR", "13F-HR", "13F-HR", "13F-HR", "13F-HR/A"),
    is_amendment = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
    ## The flag is blank on part of the real filings
    is_confidential_omitted = c(FALSE, TRUE, NA, FALSE, FALSE, TRUE),
    usable_date = as.Date(c(
      "2019-05-10", "2019-05-16", "2019-08-01", "2019-08-15", "2019-08-14", "2019-09-01"
    ))
  )
}

report_ladder_fixture <- function() {
  months <- seq(as.Date("2019-03-01"), by = "month", length.out = 6)
  tibble::tibble(
    signal = "f1", method = "cs", design = "unconditional",
    quantile = rep(1:2, each = 6), dates = rep(months, 2),
    raw_return = c(1, -1, 2, 0, 1, NA, 2, 0, 3, 1, 2, NA),
    direct_cost = c(0.05, 0, 0, 0.04, 0, 0, 0.05, 0, 0, 0.06, 0, 0),
    signal_level = rep(c(-0.01, 0.02), each = 6),
    liquidity_level = rep(c(0.4, 0.7), each = 6), n_holdings = 200
  )
}
