#' CAPM alpha of every long-short spread, next to its FF6 alpha
#'
#' The FF6 alpha removes exposure to the market, size, value, profitability, investment
#' and momentum factors. The CAPM alpha removes the market only, so the gap between the
#' two shows how much of a spread the other five factors explain. The regression mirrors
#' `summarise_backtest_returns()`: a spread is already an excess return, months without
#' factor data are dropped, and standard errors are Newey-West with Bartlett weights and
#' no prewhitening.
#'
#' @param spreads Tibble pinned as `backtests_spreads`: `signal`, `method`, `design`,
#'   `cost_bps`, `borrow_bps`, `dates` (Date, first day of the holding month) and
#'   `net_return` (percentage points per month; equal to the gross return at zero cost
#'   and borrow).
#' @param factors Tibble pinned as `bronze_ff_factors_m` (`dates`, `mkt_rf`, percentage
#'   points per month).
#' @param lag Numeric scalar, Newey-West lag.
#'
#' @return A tibble with one row per spread: `series` (named as in `backtests_summary`,
#'   `signal|method|design|spread_<cost>_<borrow>`), `n_capm_months`, `capm_alpha` (% a
#'   year), `capm_t`, `capm_lo` and `capm_hi` (95% interval, % a year) and `capm_beta`.
#' @noRd
report_capm_alphas <- function(spreads, factors, lag = 3) {
  # Validate inputs
  required_cols <- c("signal", "method", "design", "cost_bps", "borrow_bps", "dates", "net_return")
  missing_cols <- setdiff(required_cols, names(spreads))
  if (length(missing_cols) > 0) {
    stop("spreads is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  if (!all(c("dates", "mkt_rf") %in% names(factors))) {
    stop("factors must have columns dates and mkt_rf.")
  }

  one <- function(df) {
    ## Newey-West weights neighbouring residuals, so the rows must be in date order
    df <- dplyr::arrange(df, .data$dates)
    fit <- stats::lm(net_return ~ mkt_rf, data = df)
    alpha <- unname(stats::coef(fit)[1])
    se <- unname(sqrt(diag(sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE)))[1])
    tibble::tibble(
      n_capm_months = nrow(df), capm_alpha = 12 * alpha, capm_t = alpha / se,
      capm_lo = 12 * (alpha - 1.96 * se), capm_hi = 12 * (alpha + 1.96 * se),
      capm_beta = unname(stats::coef(fit)[2])
    )
  }
  spreads |>
    dplyr::filter(!is.na(.data$net_return)) |>
    dplyr::mutate(series = paste(
      .data$signal, .data$method, .data$design,
      paste0("spread_", .data$cost_bps, "_", .data$borrow_bps),
      sep = "|"
    )) |>
    dplyr::select("series", "dates", "net_return") |>
    ## Months without factor data drop out of the regression, as in the FF6 alphas
    dplyr::inner_join(dplyr::select(factors, "dates", "mkt_rf"),
      by = dplyr::join_by("dates"), relationship = "many-to-one"
    ) |>
    tidyr::nest(data = -"series") |>
    dplyr::mutate(fit = purrr::map(.data$data, one)) |>
    dplyr::select("series", "fit") |>
    tidyr::unnest("fit")
}
