#' Long-short spread returns under trading-cost and borrow scenarios
#'
#' Builds the dollar-neutral spread from a quantile ladder: long the top portfolio
#' (Qn), short the bottom one (Q1), each with weights summing to 1 (gross exposure 2,
#' plan 1.7). Trading costs of both legs come from factoRverse (its direct cost per
#' dollar traded at the fee of the run), rescaled to each scenario fee; the borrow fee is
#' charged on the short leg's holdings.
#'
#' @param ladder Tibble from `extract_ladder_returns()`: `dates` (first day of the
#'   holding month), `quantile` (1 to n), `raw_return` and `direct_cost` (percentage
#'   points), one row per quantile and month.
#' @param run_cost_bps Numeric scalar, the direct fee of the factoRverse runs in bps per
#'   dollar traded (5 for `direct_cost = 0.05`).
#' @param cost_bps Numeric vector, scenario fees in bps per dollar traded, each side.
#' @param borrow_bps Numeric vector, annual borrow fees in bps on the short leg.
#'
#' @return A tibble with one row per month and scenario: `dates`, `cost_bps`,
#'   `borrow_bps`, `gross_return` (Qn minus Q1, percentage points), `trading_cost`,
#'   `borrow_cost` and `net_return` (percentage points per month).
#'
#' @details
#' factoRverse charges `fee x |change in weight|` summed over trades, so a cost series
#' scales exactly with the fee (market impact is negligible by construction in
#' `run_quantile_ladder()`); the factoRverse validator only accepts fees below 10 bps,
#' hence the rescaling. A short sale costs the same per dollar traded as a purchase, so
#' Q1's trading cost is the short leg's. The short leg's weights sum to 1 in every
#' month, so the borrow fee is `borrow_bps / 12 / 100` percentage points per month.
#' Financing of the long leg is ignored, as stated in plan 1.7.
#' @noRd
compute_spread_returns <- function(ladder, run_cost_bps = 5, cost_bps = c(0, 10, 25, 50),
                                   borrow_bps = c(0, 100)) {
  # Validate inputs
  missing_cols <- setdiff(c("dates", "quantile", "raw_return", "direct_cost"), names(ladder))
  if (length(missing_cols) > 0) {
    stop("ladder is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  if (anyDuplicated(ladder[c("dates", "quantile")]) > 0) {
    stop("ladder must have one row per dates and quantile.")
  }

  # Top minus bottom
  top <- max(ladder$quantile)
  legs <- ladder |>
    dplyr::filter(.data$quantile %in% c(1, top)) |>
    dplyr::mutate(leg = dplyr::if_else(.data$quantile == top, "long", "short")) |>
    tidyr::pivot_wider(
      id_cols = "dates", names_from = "leg", values_from = c("raw_return", "direct_cost")
    )
  scenarios <- tidyr::expand_grid(cost_bps = cost_bps, borrow_bps = borrow_bps)
  legs |>
    dplyr::cross_join(scenarios) |>
    dplyr::transmute(
      dates = .data$dates, cost_bps = .data$cost_bps, borrow_bps = .data$borrow_bps,
      gross_return = .data$raw_return_long - .data$raw_return_short,
      trading_cost = (.data$direct_cost_long + .data$direct_cost_short) *
        .data$cost_bps / run_cost_bps,
      borrow_cost = .data$borrow_bps / 12 / 100,
      net_return = .data$gross_return - .data$trading_cost - .data$borrow_cost
    ) |>
    dplyr::arrange(.data$cost_bps, .data$borrow_bps, .data$dates)
}
