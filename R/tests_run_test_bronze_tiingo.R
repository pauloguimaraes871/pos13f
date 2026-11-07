#' Data-quality suite for the bronze Tiingo prices
#'
#' Run by [deploy_bronze_tiingo()] before the prices are pinned. It checks a per-ticker
#' summary computed by duckdb, because the full price table (tens of millions of rows)
#' does not fit in R.
#'
#' @param summary Tibble, one row per ticker and status, with `ticker`, `status`,
#'   `n_rows`, `n_dates` (distinct dates), `n_bad_close` (priced rows with a missing,
#'   zero or negative close) and `n_bad_adj` (same for the adjusted close).
#' @param tickers Tibble of requested tickers (`ticker`).
#' @param min_found_share Numeric scalar, minimum share of tickers with prices. Many
#'   OpenFIGI tickers of dead companies are unknown to Tiingo (G1 spike), so the floor
#'   is low; it catches an outage or a broken key.
#' @param min_spy_rows Numeric scalar, minimum SPY rows: SPY is the trading calendar,
#'   so it must cover the window (about 252 rows per year since 2013).
#' @param max_bad_adj_share Numeric scalar, maximum share of priced rows with a bad
#'   adjusted close.
#'
#' @return `invisible(TRUE)` when every check passes.
#'
#' @details
#' Bronze keeps prices as delivered, so it tolerates a small, known vendor defect: an
#' adjusted close of 0 where the cumulative split and dividend adjustment of a tiny OTC
#' price rounds to 0 (0.03% of rows on 2026-11-07). Silver drops those rows and applies
#' the strict checks. A non-positive raw close is never tolerated: it means a broken
#' reply, not rounding.
#' @export
run_test_bronze_tiingo <- function(summary, tickers, min_found_share = 0.4,
                                   min_spy_rows = 3000, max_bad_adj_share = 0.001) {
  testthat::test_that("every requested ticker has one result with a known status", {
    testthat::expect_length(setdiff(tickers$ticker, summary$ticker), 0)
    testthat::expect_true(all(summary$status %in% c("200", "404", "empty")))
    testthat::expect_false(any(duplicated(summary$ticker)))
  })

  testthat::test_that("prices have unique dates and positive closes", {
    priced <- dplyr::filter(summary, .data$status == "200")
    testthat::expect_equal(priced$n_dates, priced$n_rows)
    testthat::expect_true(all(priced$n_bad_close == 0))
    testthat::expect_lte(sum(priced$n_bad_adj) / sum(priced$n_rows), max_bad_adj_share)
  })

  testthat::test_that("SPY covers the window and enough tickers have prices", {
    spy <- dplyr::filter(summary, .data$ticker == "SPY", .data$status == "200")
    testthat::expect_equal(nrow(spy), 1L)
    testthat::expect_gte(spy$n_rows, min_spy_rows)
    testthat::expect_gte(mean(summary$status == "200"), min_found_share)
  })

  invisible(TRUE)
}
