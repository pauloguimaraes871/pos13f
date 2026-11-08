#' Confidence interval of the mean and minimum detectable effects of each return series
#'
#' The pre-registered economic verdict asks for the net mean return with its confidence
#' interval, and the power discussion needs the smallest effect the test could detect,
#' both for the mean return and for the FF5 + UMD alpha. All three follow from the
#' Newey-West statistics already in the summary table: the standard error of the mean is
#' `mean_m / t_mean`, and the alpha's is `alpha_se`.
#'
#' @param summary Tibble pinned as `backtests_summary` (`summarise_backtest_returns()`),
#'   with `series`, `lag`, `n_months`, `n_alpha_months`, `mean_m`, `t_mean` and
#'   `alpha_se` (percentage points per month).
#' @param lag Numeric scalar, the Newey-West lag to use (3 is primary).
#'
#' @return A tibble with one row per series: `series`, `n_months`, `n_alpha_months`,
#'   `ann_return`, `ann_return_lo` and `ann_return_hi` (95% interval of the annualised
#'   mean), `mde_mean_ann` and `mde_alpha_ann` (percentage points per year).
#'
#' @details
#' A minimum detectable effect is the true value that a two-sided 5% test would reject
#' zero for with 80% probability: (1.96 + 0.84) standard errors, times 12 to annualise.
#' A series with a zero mean has no defined `t_mean`, so its interval is `NA`.
#' @noRd
report_spread_uncertainty <- function(summary, lag = 3) {
  # Validate inputs
  required_cols <- c("series", "lag", "n_months", "n_alpha_months", "mean_m", "t_mean", "alpha_se")
  missing_cols <- setdiff(required_cols, names(summary))
  if (length(missing_cols) > 0) {
    stop("summary is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  if (!any(summary$lag == lag)) stop("summary has no rows at lag ", lag, ".")

  z <- stats::qnorm(0.975)
  power <- z + stats::qnorm(0.8)
  summary |>
    dplyr::filter(.data$lag == .env$lag) |>
    dplyr::mutate(
      ## Standard error of the monthly mean, recovered from its Newey-West t-statistic
      se_mean = dplyr::if_else(.data$t_mean != 0, abs(.data$mean_m / .data$t_mean), NA_real_),
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
