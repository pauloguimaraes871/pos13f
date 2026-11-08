#' Expected universe exclusion reasons, recomputed independently of the builder
#'
#' Re-derives each candidate's exclusion reason from its own columns, by a different
#' route than `build_gold_universe()`: every screen is evaluated as a separate failure
#' flag, the flags are stacked in a long table, and the first failing screen in the
#' documented order wins. The liquidity rank is recomputed by sorting, not by
#' `rank_desc_with_ties()`. Agreement on every row means the screens did what the plan
#' says, in the order it says.
#'
#' @param universe Tibble of universe candidates (`universe_diagnostics`): `formation_date`,
#'   `instrument_id`, `ticker`, breadth counts (`n_prev`, `n_curr`, `n_init`, `n_match`),
#'   `date` (last price day up to t), `close_prev` (USD), `first_date`, and the feature
#'   columns.
#' @param n_universe,min_price,min_history_days,max_init_share,min_holders_break,min_presence
#'   Numeric scalars, the screens of `build_gold_universe()` (same defaults).
#'
#' @return A tibble with `formation_date`, `instrument_id`, `expected_reason` (`NA` for
#'   members) and `expected_rank` (liquidity rank among eligible rows, `NA` otherwise).
#'
#' @details
#' A missing value never fails a screen by itself (a missing previous close is not "below
#' the minimum price"); missing features are caught by their own screen, as in the builder.
#' @noRd
expect_universe_reasons <- function(universe, n_universe = 1000, min_price = 5,
                                    min_history_days = 365, max_init_share = 0.5,
                                    min_holders_break = 20, min_presence = 90) {
  feature_cols <- c("close_prev", "median_volfin_3m", "mom_12_1", "rev_1m", "vol_3m", "amihud")
  screens <- c(
    "no_price_source", "identity_unverifiable", "identity_rejected", "cusip_change_suspect",
    "not_traded_at_t", "price_below_min", "low_presence", "short_history", "missing_features"
  )

  # One failure flag per screen; NA means "not failed"
  flags <- universe |>
    dplyr::transmute(
      formation_date = .data$formation_date, instrument_id = .data$instrument_id,
      median_volfin_3m = .data$median_volfin_3m,
      zero = .data$n_prev == 0 | .data$n_curr == 0,
      no_price_source = is.na(.data$ticker),
      identity_unverifiable = .data$n_curr == 0,
      identity_rejected = 2 * .data$n_match <= .data$n_curr,
      cusip_change_suspect = .data$n_curr >= min_holders_break &
        .data$n_init > max_init_share * .data$n_curr,
      not_traded_at_t = is.na(.data$date) | .data$date != .data$formation_date,
      price_below_min = .data$close_prev < min_price,
      low_presence = .data$presence < min_presence,
      short_history = as.numeric(.data$formation_date - .data$first_date) < min_history_days,
      missing_features = rowSums(is.na(dplyr::pick(dplyr::all_of(feature_cols)))) > 0
    )

  # First failing screen, from a long table of failures
  first_fail <- flags |>
    dplyr::select("formation_date", "instrument_id", dplyr::all_of(screens)) |>
    tidyr::pivot_longer(dplyr::all_of(screens), names_to = "screen", values_to = "fails") |>
    dplyr::filter(dplyr::coalesce(.data$fails, FALSE)) |>
    dplyr::mutate(order = match(.data$screen, screens)) |>
    dplyr::slice_min(.data$order, n = 1, by = c("formation_date", "instrument_id")) |>
    dplyr::select("formation_date", "instrument_id", pre_rank = "screen")

  # Rank the eligible by sorting, then the rank cut and the zero-holder rule
  flags |>
    dplyr::left_join(first_fail,
      by = dplyr::join_by("formation_date", "instrument_id"), relationship = "one-to-one"
    ) |>
    dplyr::arrange(
      .data$formation_date, dplyr::desc(.data$median_volfin_3m), .data$instrument_id
    ) |>
    dplyr::mutate(
      expected_rank = dplyr::if_else(
        is.na(.data$pre_rank), cumsum(is.na(.data$pre_rank)), NA_integer_
      ),
      .by = "formation_date"
    ) |>
    dplyr::mutate(expected_reason = dplyr::case_when(
      !is.na(.data$pre_rank) ~ .data$pre_rank,
      .data$expected_rank > n_universe ~ "below_liquidity_rank",
      .data$zero ~ "zero_holders",
      .default = NA_character_
    )) |>
    dplyr::select("formation_date", "instrument_id", "expected_reason", "expected_rank")
}
