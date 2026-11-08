#' Data-quality suite for the gold panels
#'
#' Run by `deploy_gold()` before anything is pinned. Checks the factoRverse
#' meta_dataframe contract (so `run_port_backtest()` accepts the panels as they are),
#' the frozen quarterly design, the plausibility of signal, cohort, universe and
#' returns, that every universe and cohort screen did what the plan says
#' (`check_gold_universe_filters()`, `check_gold_cohort_filters()`), and that a random
#' sample of panel rows matches a dplyr recomputation from silver
#' (`check_gold_reference()`).
#'
#' @param panels Named list from `build_gold_panels()`.
#' @param cohort Tibble from `select_gold_cohort()`.
#' @param benchmark_d Tibble of daily SPY returns (`date`, `bench_return`).
#' @param benchmark_m Tibble of monthly SPY returns (`dates`, `bench_return_1m`).
#' @param formations Tibble from `build_formation_dates()`.
#' @param filings Tibble pinned as `silver_filings`.
#' @param versions Tibble pinned as `silver_holdings_versions`.
#' @param relations Tibble pinned as `silver_filing_relations`.
#' @param reference Named list from `build_gold_reference()`.
#' @param n_universe Numeric scalar, the universe size of plan 1.8.
#' @param min_price,min_history_days,min_presence,min_positions Numeric scalars, the
#'   screens of `build_gold_universe()` and `select_gold_cohort()` (5 USD, 365 days, 90
#'   percent of traded days, 10 positions).
#' @param max_membership_turnover,max_reentry_share Numeric scalars, bounds on how
#'   fast universe membership changes (see `check_gold_universe_filters()`).
#' @param min_universe_share Numeric scalar, smallest universe as a share of
#'   `n_universe` (the zero-holder exclusion D5 removes a few names).
#' @param max_cohort_change Numeric scalar, largest relative change of the cohort size
#'   between formations.
#'
#' @return `invisible(TRUE)` when every check passes.
#' @export
run_test_gold <- function(panels, cohort, benchmark_d, benchmark_m, formations, filings,
                          versions, relations, reference, n_universe = 1000,
                          min_universe_share = 0.9, max_cohort_change = 0.25, min_price = 5,
                          min_history_days = 365, min_presence = 90, min_positions = 10,
                          max_membership_turnover = 0.15,
                          max_reentry_share = 0.03) {
  key <- c("id", "tickers", "dates")
  m_dfs <- panels[c("signals_m_df", "fwd_1m_returns_m_df", "liquidity_m_df", "volatility_m_df")]
  signals <- panels$signals_m_df
  returns <- panels$fwd_1m_returns_m_df

  testthat::test_that("every panel follows the meta_dataframe contract", {
    for (m in m_dfs) {
      testthat::expect_identical(names(m)[1:3], key)
      testthat::expect_identical(m$id, paste0(m$tickers, "-", m$dates))
      testthat::expect_identical(m$id, dplyr::arrange(m, .data$id)$id)
      testthat::expect_false(anyDuplicated(m$id) > 0)
      testthat::expect_true(all(purrr::map_lgl(m[-(1:3)], is.numeric)))
      ## Inf or NaN would pass an NA check and break every rank and weight downstream
      finite <- purrr::map_lgl(m[-(1:3)], \(x) all(is.finite(x) | is.na(x) & !is.nan(x)))
      testthat::expect_true(all(finite))
    }
  })

  testthat::test_that("liquidity used for trading costs is positive on every row", {
    ## factoRverse divides each order by mean_volfin_3m: zero gives NaN costs
    testthat::expect_true(all(panels$liquidity_m_df$mean_volfin_3m > 0))
    testthat::expect_true(all(panels$volatility_m_df$daily_vol >= 0))
  })

  testthat::test_that("panels share rows and only unfinished returns are missing", {
    for (m in m_dfs) testthat::expect_identical(m$id, signals$id)
    for (m in m_dfs[-2]) testthat::expect_false(anyNA(m))
    missing_dates <- unique(returns$dates[is.na(returns$fwd_return_1m)])
    testthat::expect_true(length(missing_dates) <= 1)
    testthat::expect_true(all(missing_dates == max(returns$dates)))
  })

  testthat::test_that("dates are first days of consecutive months", {
    dates <- sort(unique(signals$dates))
    testthat::expect_true(all(lubridate::mday(dates) == 1))
    testthat::expect_true(all(diff(lubridate::year(dates) * 12 + lubridate::month(dates)) == 1))
  })

  testthat::test_that("each formation's signal is frozen over its holding months", {
    frozen <- signals |>
      ## Month index of the formation label: holding month minus the signal's age
      dplyr::mutate(
        formation_label = lubridate::year(.data$dates) * 12 +
          lubridate::month(.data$dates) - .data$signal_age_months
      ) |>
      dplyr::summarise(
        n = dplyr::n_distinct(.data$f1_breadth_change), .by = c("tickers", "formation_label")
      )
    testthat::expect_true(all(frozen$n == 1))
    testthat::expect_true(all(signals$signal_age_months %in% 0:2))
  })

  testthat::test_that("universe and cohort have plausible, stable sizes", {
    sizes <- dplyr::count(dplyr::filter(signals, .data$signal_age_months == 0), .data$dates)
    testthat::expect_true(all(sizes$n <= n_universe))
    testthat::expect_true(all(sizes$n >= min_universe_share * n_universe))
    per_formation <- dplyr::summarise(cohort, n = sum(.data$in_cohort), .by = "formation_date")
    change <- abs(diff(per_formation$n) / utils::head(per_formation$n, -1))
    testthat::expect_true(all(change <= max_cohort_change))
  })

  testthat::test_that("signal, breadth and returns are bounded", {
    testthat::expect_true(all(abs(signals$f1_breadth_change) <= 1))
    testthat::expect_true(all(signals$breadth_level > 0 & signals$breadth_level <= 1))
    r <- returns$fwd_return_1m[!is.na(returns$fwd_return_1m)]
    testthat::expect_true(all(r > -100))
    testthat::expect_lte(mean(abs(r) > 100), 0.005)
    s <- returns[!is.na(returns$fwd_return_1m), ]
    testthat::expect_true(all(s$fwd_return_1m_stress <= s$fwd_return_1m + 1e-9))
  })

  testthat::test_that("the benchmark covers every panel month and day", {
    finished <- unique(returns$dates[!is.na(returns$fwd_return_1m)])
    testthat::expect_true(all(finished %in% benchmark_m$dates))
    testthat::expect_false(anyNA(benchmark_d$bench_return))
    testthat::expect_false(is.unsorted(benchmark_d$date, strictly = TRUE))
    recompounded <- benchmark_d |>
      dplyr::mutate(dates = lubridate::floor_date(.data$date, "month")) |>
      dplyr::summarise(r = 100 * (prod(1 + .data$bench_return / 100) - 1), .by = "dates")
    testthat::expect_equal(benchmark_m$bench_return_1m, recompounded$r)
  })

  check_gold_universe_filters(
    panels, n_universe, min_price, min_history_days, min_presence,
    max_membership_turnover, max_reentry_share
  )
  check_gold_cohort_filters(cohort, filings, versions, relations, formations, min_positions)
  check_gold_reference(panels, cohort, reference)

  invisible(TRUE)
}
