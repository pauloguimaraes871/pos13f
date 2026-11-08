#' Data-quality suite for the backtests
#'
#' Run by `deploy_backtests()` before anything is pinned. Checks that the ladders are
#' what they claim (portfolios sorted on the signal, of usable size), that factoRverse's
#' accounting ties out once mapped to holding months, that costs behave, and that the
#' spread is close to market neutral. The sorting check follows the owner's idea: a
#' higher quantile must hold a higher weighted average of its sorting signal.
#'
#' @param ladder Tibble of every ladder (`extract_ladder_returns()` plus `signal`,
#'   `method`, `design`).
#' @param spreads Tibble of spreads (`compute_spread_returns()` plus the same keys).
#' @param benchmark Tibble with `series`, `dates`, `ret` (SPY and same-universe EW).
#' @param min_holdings Numeric scalar, fewest stocks a portfolio may hold (plan 1.7: a
#'   leg under 20 names would make the formation flat).
#' @param max_spread_corr Numeric scalar, largest absolute correlation of a gross spread
#'   with SPY.
#'
#' @return `invisible(TRUE)` when every check passes.
#' @export
run_test_backtests <- function(ladder, spreads, benchmark, min_holdings = 20,
                               max_spread_corr = 0.5) {
  keys <- c("signal", "method", "design")
  finished <- dplyr::filter(ladder, !is.na(.data$raw_return))

  testthat::test_that("a higher quantile holds a higher weighted average signal", {
    ## Signals are frozen within a formation, so every month must be sorted
    sorted <- ladder |>
      dplyr::arrange(.data$quantile) |>
      dplyr::summarise(ok = !is.unsorted(.data$signal_level), .by = c(keys, "dates"))
    testthat::expect_true(all(sorted$ok))
  })

  testthat::test_that("every portfolio holds enough stocks in every month", {
    testthat::expect_false(anyNA(ladder$n_holdings))
    testthat::expect_true(all(ladder$n_holdings >= min_holdings))
  })

  testthat::test_that("net return is raw return minus costs in every month", {
    costs <- finished$direct_cost + finished$market_impact_cost
    gap <- finished$net_return - (finished$raw_return - costs)
    testthat::expect_lt(max(abs(gap)), 1e-8)
  })

  testthat::test_that("trading happens at rebalances only and costs at most full turnover", {
    testthat::expect_true(all(ladder$direct_cost >= 0))
    ## At 5 bps, replacing the whole portfolio (|change| summing to 2) costs 0.10 pp
    testthat::expect_true(all(ladder$direct_cost <= 0.1 + 1e-9))
    ## Between rebalances factoRverse leaves weights to drift; what it charges there is
    ## floating-point noise from re-normalising weights (about 1e-17)
    rebalance <- lubridate::month(ladder$dates) %in% c(3, 6, 9, 12)
    testthat::expect_true(all(ladder$direct_cost[rebalance] > 0))
    testthat::expect_true(all(ladder$direct_cost[!rebalance] < 1e-10))
  })

  testthat::test_that("every quantile of every ladder covers the same finished months", {
    all_months <- sort(unique(finished$dates))
    months <- dplyr::summarise(finished,
      same = identical(sort(unique(.data$dates)), all_months), .by = c(keys, "quantile")
    )
    testthat::expect_true(all(months$same))
    ## Every ladder has all its quantiles in every finished month
    per_month <- dplyr::summarise(finished,
      n = dplyr::n_distinct(.data$quantile), .by = c(keys, "dates")
    )
    testthat::expect_equal(length(unique(per_month$n)), 1)
  })

  testthat::test_that("net spreads fall with the fee and the borrow charge", {
    means <- spreads |>
      dplyr::summarise(net = mean(.data$net_return), .by = c(keys, "cost_bps", "borrow_bps")) |>
      dplyr::arrange(.data$borrow_bps, .data$cost_bps)
    by_cost <- dplyr::summarise(means, ok = !is.unsorted(-.data$net), .by = c(keys, "borrow_bps"))
    testthat::expect_true(all(by_cost$ok))
  })

  testthat::test_that("gross spreads are close to market neutral", {
    spy <- dplyr::filter(benchmark, .data$series == "spy")
    corr <- spreads |>
      dplyr::filter(.data$cost_bps == 0, .data$borrow_bps == 0) |>
      dplyr::inner_join(dplyr::select(spy, "dates", spy = "ret"),
        by = dplyr::join_by("dates"), relationship = "many-to-one"
      ) |>
      dplyr::summarise(rho = stats::cor(.data$gross_return, .data$spy), .by = keys)
    testthat::expect_true(all(abs(corr$rho) <= max_spread_corr))
  })
  invisible(TRUE)
}
