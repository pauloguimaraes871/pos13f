#' Momentum over the study window against its long history
#'
#' The UMD factor is the yardstick for reading a weak F1 result: in a decade when
#' momentum itself was not significant, a non-rejection is weak evidence either way.
#'
#' @param factors Tibble pinned as `bronze_ff_factors_m` (`dates`, `mkt_rf`, `umd`,
#'   percentage points per month).
#' @param first_month,last_month Date scalars, the study window.
#' @param lag Numeric scalar, Newey-West lag.
#'
#' @return A list: `umd` (tibble with `period`, `n_months`, `ann_mean`, `ann_vol`,
#'   `t_mean`, `capm_alpha` and `capm_t`, for the study window and since 1963) and
#'   `umd_by_year` (tibble with `year` and `umd_return`, compounded, percent).
#' @noRd
report_momentum_context <- function(factors, first_month = as.Date("2014-03-01"),
                                    last_month = as.Date("2026-09-01"), lag = 3) {
  nw_t <- function(fit) {
    stats::coef(fit) / sqrt(diag(sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE)))
  }
  one <- function(df, period) {
    mean_fit <- stats::lm(umd ~ 1, data = df)
    capm <- stats::lm(umd ~ mkt_rf, data = df)
    tibble::tibble(
      period = period, n_months = nrow(df), ann_mean = 12 * mean(df$umd),
      ann_vol = sqrt(12) * stats::sd(df$umd), t_mean = unname(nw_t(mean_fit)),
      capm_alpha = 12 * unname(stats::coef(capm)[1]), capm_t = unname(nw_t(capm)[1])
    )
  }
  window <- dplyr::filter(factors, .data$dates >= first_month, .data$dates <= last_month)
  list(
    umd = dplyr::bind_rows(one(window, "study window"), one(factors, "since 1963")),
    umd_by_year = window |>
      dplyr::mutate(year = lubridate::year(.data$dates)) |>
      dplyr::summarise(umd_return = 100 * (prod(1 + .data$umd / 100) - 1), .by = "year")
  )
}
