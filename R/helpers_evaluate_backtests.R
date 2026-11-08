#' Evaluate the backtest ladders: spreads, benchmarks, summaries and diagnostics
#'
#' @param ladder Tibble of every ladder from `extract_ladder_returns()`, with `signal`,
#'   `method` and `design` (`"unconditional"` or `"tercile_<k>"`).
#' @param inputs Named list from `build_backtest_inputs()`.
#' @param factors Tibble pinned as `bronze_ff_factors_m`.
#' @param board A `pins_board` (reads `gold_fwd_1m_returns_m_df` and `gold_benchmark_m`).
#' @param signals Character vector, signals whose rank IC is computed.
#'
#' @return A named list of tibbles: `spreads` (every design, plus `tercile_avg`, the
#'   equal average of the three tercile spreads), `benchmark` (SPY and the same-universe
#'   equal weight, monthly), `summary` (`summarise_backtest_returns()` of portfolios,
#'   spreads and benchmarks), `rank_ic`, `rank_ic_by_formation`, `monotonicity` and
#'   `inference`.
#'
#' @details
#' Only finished months enter (the calendar's last month has no forward return). The
#' long-only portfolios and benchmarks are regressed in excess of the risk-free rate;
#' spreads are already excess returns.
#' @noRd
evaluate_backtests <- function(ladder, inputs, factors, board, signals) {
  keys <- c("signal", "method", "design")
  finished <- dplyr::filter(ladder, !is.na(.data$raw_return))
  spreads <- finished |>
    tidyr::nest(data = -dplyr::all_of(keys)) |>
    dplyr::mutate(data = purrr::map(.data$data, compute_spread_returns)) |>
    tidyr::unnest("data")
  tercile_avg <- spreads |>
    dplyr::filter(startsWith(.data$design, "tercile_")) |>
    dplyr::summarise(
      dplyr::across(c("gross_return", "trading_cost", "borrow_cost", "net_return"), mean),
      .by = c("signal", "method", "dates", "cost_bps", "borrow_bps")
    ) |>
    dplyr::mutate(design = "tercile_avg")
  spreads <- dplyr::bind_rows(spreads, tercile_avg)

  # Benchmarks: SPY and the same-universe equal weight
  fwd <- tibble::as_tibble(pins::pin_read(board, "gold_fwd_1m_returns_m_df"))
  benchmark <- dplyr::bind_rows(
    tibble::as_tibble(pins::pin_read(board, "gold_benchmark_m")) |>
      dplyr::transmute(
        series = "spy", dates = as.Date(as.numeric(.data$dates)), ret = .data$bench_return_1m
      ),
    fwd |>
      dplyr::filter(!is.na(.data$fwd_return_1m)) |>
      dplyr::summarise(ret = mean(.data$fwd_return_1m), .by = "dates") |>
      dplyr::mutate(series = "ew_universe", dates = as.Date(as.numeric(.data$dates)))
  ) |>
    dplyr::semi_join(dplyr::distinct(finished, .data$dates), by = dplyr::join_by("dates"))

  # Series for the summary
  name <- function(df) paste(df$signal, df$method, df$design, sep = "|")
  portfolios <- dplyr::transmute(finished,
    series = paste0(name(finished), "|q", .data$quantile), dates = .data$dates,
    ret = .data$raw_return
  )
  spread_series <- spreads |>
    dplyr::filter(
      (.data$cost_bps == 0 & .data$borrow_bps == 0) |
        (.data$cost_bps == 25 & .data$borrow_bps == 100)
    ) |>
    dplyr::transmute(
      series = paste(.data$signal, .data$method, .data$design,
        paste0("spread_", .data$cost_bps, "_", .data$borrow_bps),
        sep = "|"
      ),
      dates = .data$dates, ret = .data$net_return
    )
  long_only <- c(unique(portfolios$series), unique(benchmark$series))
  summary <- summarise_backtest_returns(
    dplyr::bind_rows(portfolios, spread_series, dplyr::select(benchmark, "series", "dates", "ret")),
    factors,
    excess_series = long_only
  )

  ic <- purrr::map(signals, \(s) compute_rank_ic(inputs$signals, fwd, s))
  unconditional <- dplyr::filter(finished, .data$design == "unconditional")
  list(
    spreads = spreads, benchmark = benchmark, summary = summary,
    rank_ic = dplyr::bind_rows(
      purrr::map2(ic, signals, \(x, s) dplyr::mutate(x$summary, signal = s))
    ),
    rank_ic_by_formation = dplyr::bind_rows(
      purrr::map2(ic, signals, \(x, s) dplyr::mutate(x$by_formation, signal = s))
    ),
    monotonicity = unconditional |>
      tidyr::nest(data = -dplyr::all_of(keys)) |>
      dplyr::mutate(test = purrr::map(.data$data, test_monotonicity)) |>
      dplyr::select(-"data") |>
      tidyr::unnest("test") |>
      dplyr::mutate(dplyr::across(
        c("diffs", "t_stats"), \(col) purrr::map_chr(col, \(d) paste(round(d, 4), collapse = ", "))
      )),
    inference = spreads |>
      tidyr::nest(data = -dplyr::all_of(keys)) |>
      dplyr::mutate(inf = purrr::map(.data$data, compute_spread_inference)) |>
      dplyr::select(-"data") |>
      tidyr::unnest("inf")
  )
}
