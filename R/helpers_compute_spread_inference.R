#' Break-even cost, minimum detectable effect and probabilistic Sharpe of a spread
#'
#' Three numbers the memo reports next to the spread (Sections 7 and 8): the trading fee
#' at which the net spread earns nothing, the smallest annual mean the test could detect
#' with 80% power at 5% two-sided given the spread's Newey-West standard error, and the
#' probability that the true Sharpe ratio is above zero given the sample's skewness and
#' kurtosis (the deflated Sharpe ratio of Bailey and Lopez de Prado with one trial)
#' \[mem\].
#'
#' @param spreads Tibble from `compute_spread_returns()`.
#' @param lag Numeric scalar, Newey-West lag for the standard error.
#'
#' @return A tibble with `breakeven_bps` (fee in bps per dollar traded, each side, with no
#'   borrow fee), `mde_ann` (percentage points per year), `psr` (probability) and
#'   `n_months`.
#' @noRd
compute_spread_inference <- function(spreads, lag = 3) {
  base <- dplyr::filter(spreads, .data$borrow_bps == 0)
  gross <- dplyr::filter(base, .data$cost_bps == 0)
  ## Costs are linear in the fee: mean cost per bp from any positive scenario
  scenario <- dplyr::filter(base, .data$cost_bps > 0)
  cost_per_bp <- mean(scenario$trading_cost / scenario$cost_bps)
  fit <- stats::lm(gross_return ~ 1, data = gross)
  se <- sqrt(sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE))[1]
  r <- gross$gross_return
  n <- length(r)
  sr <- mean(r) / stats::sd(r)
  skew <- mean((r - mean(r))^3) / stats::sd(r)^3
  kurt <- mean((r - mean(r))^4) / stats::sd(r)^4
  tibble::tibble(
    breakeven_bps = mean(r) / cost_per_bp,
    mde_ann = 12 * (stats::qnorm(0.975) + stats::qnorm(0.8)) * se,
    psr = stats::pnorm(sr * sqrt(n - 1) / sqrt(1 - skew * sr + (kurt - 1) / 4 * sr^2)),
    n_months = n
  )
}
