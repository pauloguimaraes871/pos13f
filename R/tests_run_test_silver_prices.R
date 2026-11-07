#' Data-quality suite for the silver identity and price tables
#'
#' Run by `deploy_silver_prices()` before anything is pinned. Strict on keys, dates and
#' identity; tolerant only where real markets are noisy (extreme returns), and then
#' measured on liquid stocks, the ones the study can trade.
#'
#' @param master Tibble from `build_silver_security_master()`.
#' @param checks One-row tibble from `summarise_silver_prices_checks()`.
#' @param max_big_liquid Numeric scalar, largest share of liquid one-day returns beyond
#'   +-50% (0.012% on 2026-11-07).
#' @param min_match_liquid Numeric scalar, smallest share of liquid, well-held
#'   CUSIP-quarters whose filers' implied price agrees with Tiingo's close (99.96% on
#'   2026-11-07).
#' @param min_total_return_agree Numeric scalar, smallest share of dividend and split
#'   days whose adjusted return equals the total return rebuilt from raw data (99.9% on
#'   2026-11-07). A vendor switching to price-only adjustment would fall to about 3%.
#'
#' @return `invisible(TRUE)` when every check passes.
#'
#' @details
#' The AAPL specimen guards the use of adjusted prices: on 2020-08-31 (4-for-1 split)
#' the raw close fell from 499.23 to 129.04, while the total return was +3.39%. A raw
#' price slipping into returns would show -74%.
#' @export
run_test_silver_prices <- function(master, checks, max_big_liquid = 5e-4,
                                   min_match_liquid = 0.99, min_total_return_agree = 0.995) {
  testthat::test_that("the security master maps each CUSIP to one instrument", {
    testthat::expect_false(anyDuplicated(master$cusip) > 0)
    testthat::expect_false(anyNA(master$instrument_id))
    priced <- dplyr::filter(master, .data$has_price_source)
    per_ticker <- dplyr::summarise(priced,
      n = dplyr::n_distinct(.data$instrument_id), .by = "ticker"
    )
    testthat::expect_true(all(per_ticker$n == 1))
  })

  testthat::test_that("prices are unique, positive and on trading days", {
    testthat::expect_equal(checks$n_duplicate_ids, 0)
    testthat::expect_equal(checks$n_nonpositive, 0)
    testthat::expect_equal(checks$n_off_calendar, 0)
    testthat::expect_equal(checks$n_tickers_multi, 0)
  })

  testthat::test_that("returns, flags and terminal events cover every price", {
    ## One return per price except each instrument's first
    testthat::expect_equal(checks$n_returns, checks$n_prices - checks$n_instruments)
    testthat::expect_equal(checks$n_liquidity, checks$n_prices)
    testthat::expect_equal(checks$n_missing_flags, 0)
    testthat::expect_equal(checks$n_terminal, checks$n_instruments)
    testthat::expect_equal(checks$n_after_calendar, 0)
  })

  testthat::test_that("returns are adjusted and rarely extreme among liquid stocks", {
    testthat::expect_equal(checks$aapl_split_ret, 0.0339, tolerance = 0.01)
    ## Dividends and splits are inside the adjusted return (total return, not price)
    testthat::expect_gte(checks$share_total_return_agree, min_total_return_agree)
    testthat::expect_lte(checks$share_big_liquid, max_big_liquid)
  })

  testthat::test_that("13F values agree with prices for liquid, well-held stocks", {
    ## Ticker reuse would attach another company's prices: the implied price disagrees
    testthat::expect_gte(checks$share_match_liquid, min_match_liquid)
  })

  invisible(TRUE)
}
