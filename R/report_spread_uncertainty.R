#' Confidence interval of the mean and minimum detectable effects of each return series
#'
#' The pre-registered economic verdict asks for the net mean return with its confidence
#' interval, and the power discussion needs the smallest effect the test could detect,
#' both for the mean return and for the FF5 + UMD alpha. The alpha's standard error is
#' in the summary table. The mean's is computed directly from the monthly returns for
#' every long-short spread (Newey-West, in date order), and recovered as
#' `mean_m / t_mean` for the other series (portfolios and benchmarks).
#'
#' @param summary Tibble pinned as `backtests_summary` (`summarise_backtest_returns()`),
#'   with `series`, `lag`, `n_months`, `n_alpha_months`, `mean_m`, `t_mean` and
#'   `alpha_se` (percentage points per month).
#' @param spreads Tibble pinned as `backtests_spreads`: `signal`, `method`, `design`,
#'   `cost_bps`, `borrow_bps`, `dates` and `net_return` (percentage points per month;
#'   the gross return at zero cost and borrow). Its series are named as in `summary`.
#' @param lag Numeric scalar, the Newey-West lag to use (3 is primary).
#'
#' @return A tibble with one row per series: `series`, `n_months`, `n_alpha_months`,
#'   `ann_return`, `ann_return_lo` and `ann_return_hi` (95% interval of the annualised
#'   mean), `mde_mean_ann` and `mde_alpha_ann` (percentage points per year).
#'
#' @details
#' A minimum detectable effect is the true value that a two-sided 5% test would reject
#' zero for with 80% probability: (1.96 + 0.84) standard errors, times 12 to annualise.
#' For a series outside `spreads` whose mean is exactly zero, `t_mean` is zero and the
#' standard error cannot be recovered, so its interval is `NA`.
#' @noRd
report_spread_uncertainty <- function(summary, spreads, lag = 3) {
  # Validate inputs
  required_cols <- c(
    "series", "lag", "n_months", "n_alpha_months", "mean_m", "t_mean", "alpha_se"
  )
  missing_cols <- setdiff(required_cols, names(summary))
  if (length(missing_cols) > 0) {
    stop("summary is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  spread_cols <- c(
    "signal", "method", "design", "cost_bps", "borrow_bps", "dates", "net_return"
  )
  missing_cols <- setdiff(spread_cols, names(spreads))
  if (length(missing_cols) > 0) {
    stop("spreads is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  if (!any(summary$lag == lag)) stop("summary has no rows at lag ", lag, ".")

  # Standard error of each spread's monthly mean, from its returns
  ## Newey-West weights neighbouring months, so each series is put in date order first
  nw_se <- function(r) {
    sqrt(sandwich::NeweyWest(stats::lm(r ~ 1), lag = lag, prewhite = FALSE))[1]
  }
  spread_se <- spreads |>
    dplyr::filter(!is.na(.data$net_return)) |>
    dplyr::mutate(series = paste(
      .data$signal, .data$method, .data$design,
      paste0("spread_", .data$cost_bps, "_", .data$borrow_bps),
      sep = "|"
    )) |>
    dplyr::arrange(.data$series, .data$dates) |>
    dplyr::summarise(se_spread = nw_se(.data$net_return), .by = "series")

  # Interval and detectable effects
  z <- stats::qnorm(0.975)
  power <- z + stats::qnorm(0.8)
  summary |>
    dplyr::filter(.data$lag == .env$lag) |>
    dplyr::left_join(spread_se, by = dplyr::join_by("series"), relationship = "one-to-one") |>
    dplyr::mutate(
      ## Other series: the error recovered from the Newey-West t-statistic of the mean
      se_recovered = dplyr::if_else(
        .data$t_mean != 0, abs(.data$mean_m / .data$t_mean), NA_real_
      ),
      se_mean = dplyr::coalesce(.data$se_spread, .data$se_recovered),
      ann_return = 12 * .data$mean_m,
      ann_return_lo = 12 * (.data$mean_m - z * .data$se_mean),
      ann_return_hi = 12 * (.data$mean_m + z * .data$se_mean),
      mde_mean_ann = 12 * power * .data$se_mean,
      mde_alpha_ann = 12 * power * .data$alpha_se
    ) |>
    dplyr::select(
      "series", "n_months", "n_alpha_months", "ann_return", "ann_return_lo",
      "ann_return_hi", "mde_mean_ann", "mde_alpha_ann"
    )
}
