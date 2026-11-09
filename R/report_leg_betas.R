#' Market beta of each quantile portfolio of a ladder
#'
#' The long-short spread is dollar neutral; whether it is also close to market neutral
#' depends on the two legs having similar betas. This regresses each quantile's monthly
#' excess return on the market factor.
#'
#' @param ladder Tibble pinned as `backtests_ladder_returns`: `signal`, `method`,
#'   `design`, `quantile`, `dates` (Date, first day of the holding month) and
#'   `raw_return` (percentage points per month).
#' @param factors Tibble pinned as `bronze_ff_factors_m` (`dates`, `mkt_rf`, `rf`,
#'   percentage points per month).
#'
#' @return A tibble with one row per `signal`, `method`, `design` and `quantile`:
#'   `n_months` and `beta`.
#' @noRd
report_leg_betas <- function(ladder, factors) {
  # Validate inputs
  required_cols <- c("signal", "method", "design", "quantile", "dates", "raw_return")
  missing_cols <- setdiff(required_cols, names(ladder))
  if (length(missing_cols) > 0) {
    stop("ladder is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  if (!all(c("dates", "mkt_rf", "rf") %in% names(factors))) {
    stop("factors must have columns dates, mkt_rf and rf.")
  }

  ladder |>
    dplyr::filter(!is.na(.data$raw_return)) |>
    ## Months without factor data drop out, as in the alpha regressions
    dplyr::inner_join(dplyr::select(factors, "dates", "mkt_rf", "rf"),
      by = dplyr::join_by("dates"), relationship = "many-to-one"
    ) |>
    dplyr::summarise(
      n_months = dplyr::n(),
      beta = unname(stats::coef(stats::lm((.data$raw_return - .data$rf) ~ .data$mkt_rf))[2]),
      .by = c("signal", "method", "design", "quantile")
    )
}
