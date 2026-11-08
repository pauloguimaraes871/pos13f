#' Performance, Newey-West inference and Fama-French alphas of monthly return series
#'
#' The single evaluation helper of the backtests: for each series it reports the mean
#' and volatility, the Newey-West t-statistic of the mean, and the FF5 + UMD alpha with
#' its Newey-West standard error and 95% interval, at each lag (memo Section 7: 3 is
#' primary, 6 a sensitivity).
#'
#' @param returns Tibble with `series` (chr), `dates` (Date, first day of the month) and
#'   `ret` (percentage points per month), one row per series and month.
#' @param factors Tibble pinned as `bronze_ff_factors_m` (`dates`, `mkt_rf`, `smb`,
#'   `hml`, `rmw`, `cma`, `umd`, `rf`, percentage points).
#' @param excess_series Character vector, series that are long-only portfolios: their
#'   alpha regression uses returns in excess of `rf`. Spreads are already excess returns.
#' @param lags Numeric vector, Newey-West lags in months.
#'
#' @return A tibble with one row per series and lag: `series`, `lag`, `n_months`,
#'   `mean_m`, `ann_return` (12 x mean), `ann_vol`, `sharpe` (annualised, of `ret`),
#'   `t_mean`, `alpha_m`, `alpha_se`, `alpha_t`, `alpha_lo`, `alpha_hi` (95% interval,
#'   percentage points per month) and `ann_alpha`.
#'
#' @details
#' Months without factor data (the library lags by a few months) are dropped from the
#' alpha regression only, and `n_alpha_months` records how many remain. Standard errors
#' use `sandwich::NeweyWest()` with Bartlett weights, no prewhitening.
#' @noRd
summarise_backtest_returns <- function(returns, factors, excess_series = character(),
                                       lags = c(3, 6)) {
  # Validate inputs
  missing_cols <- setdiff(c("series", "dates", "ret"), names(returns))
  if (length(missing_cols) > 0) {
    stop("returns is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  if (anyNA(returns$ret)) stop("returns$ret must not be NA: drop unfinished months first.")

  nw_se <- function(fit, lag) sqrt(diag(sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE)))
  one <- function(df, series, lag) {
    mean_fit <- stats::lm(ret ~ 1, data = df)
    reg <- df |>
      dplyr::inner_join(factors, by = dplyr::join_by("dates"), relationship = "one-to-one") |>
      dplyr::mutate(y = .data$ret - if (series %in% excess_series) .data$rf else 0)
    alpha_fit <- stats::lm(y ~ mkt_rf + smb + hml + rmw + cma + umd, data = reg)
    alpha <- unname(stats::coef(alpha_fit)[1])
    se <- unname(nw_se(alpha_fit, lag)[1])
    tibble::tibble(
      series = series, lag = lag, n_months = nrow(df), n_alpha_months = nrow(reg),
      mean_m = mean(df$ret), ann_return = 12 * mean(df$ret),
      ann_vol = sqrt(12) * stats::sd(df$ret), sharpe = sqrt(12) * mean(df$ret) / stats::sd(df$ret),
      t_mean = mean(df$ret) / unname(nw_se(mean_fit, lag)),
      alpha_m = alpha, alpha_se = se, alpha_t = alpha / se,
      alpha_lo = alpha - 1.96 * se, alpha_hi = alpha + 1.96 * se, ann_alpha = 12 * alpha
    )
  }
  returns |>
    dplyr::arrange(.data$series, .data$dates) |>
    tidyr::nest(data = -"series") |>
    tidyr::expand_grid(lag = lags) |>
    purrr::pmap(\(series, data, lag) one(data, series, lag)) |>
    dplyr::bind_rows()
}
