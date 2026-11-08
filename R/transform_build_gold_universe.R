#' Select the liquid universe of each formation, from data up to t-1
#'
#' Plan 1.8 with memo deviations D2 and D5, applied in this order to the instruments
#' the cohort held in q-1 or q: an attributable price source; identity accepted at t
#' (most quarter-q cohort holders report a value per share near Tiingo's close); traded
#' on t (the portfolio executes at the close of t); previous close of at least
#' `min_price`; traded (positive dollar volume) on at least `min_presence` percent of
#' the previous 63 trading days; at least `min_history_days` of price history; complete
#' features; then the top `n_universe` by previous 63-day median dollar volume (ties by
#' `instrument_id`); finally stocks with zero cohort holders in q-1 or q leave (D5: a
#' CUSIP change would otherwise create false initiations and exits).
#'
#' @param formations Tibble from `build_formation_dates()`.
#' @param breadth Tibble from `sql_gold_breadth()`.
#' @param features Tibble from `sql_gold_monthly_features()`.
#' @param terminal Tibble pinned as `silver_terminal_events` (`instrument_id`,
#'   `first_date`).
#' @param master Tibble pinned as `silver_security_master` (`instrument_id`, `ticker`,
#'   `has_price_source`).
#' @param n_universe,min_price,min_history_days Numeric scalars (plan 1.8: 1000, 5 USD,
#'   12 months).
#' @param min_presence Numeric scalar, percent of the previous 63 trading days with
#'   positive dollar volume (owner decision 2026-11-08: 90). Tiingo keeps carrying a
#'   frozen price with zero volume after some cash takeovers (AL in 2026); without this
#'   screen such a stale stock entered the universe on its earlier volume. A member that
#'   passes keeps positive 63-day volume through its three holding months.
#' @param max_init_share,min_holders_break Numeric scalars: an instrument with at least
#'   `min_holders_break` quarter-q holders, more than `max_init_share` of them new, is a
#'   suspected CUSIP change (memo deviation D9). On 2026-11-07 the largest F1 values were
#'   all such events (GOOGL 2016, RTX 2020, GE 2021, MDT 2015, BKNG 2018, BLK 2025):
#'   holders moving from an old CUSIP that maps to another FIGI look like initiations.
#'   Plan 1.6 treats a CUSIP change creating a new instrument as an identity break that
#'   leaves the signal for that formation.
#'
#' @return A tibble with one row per formation and candidate instrument, ordered by
#'   `formation_date` and `instrument_id`: the breadth counts, the features at t,
#'   `ticker`, `liquidity_rank`, `excluded_reason` (`NA` for members) and
#'   `in_universe` (lgl).
#' @noRd
build_gold_universe <- function(formations, breadth, features, terminal, master,
                                n_universe = 1000, min_price = 5, min_history_days = 365,
                                min_presence = 90,
                                max_init_share = 0.5, min_holders_break = 20) {
  feature_cols <- c("close_prev", "median_volfin_3m", "mom_12_1", "rev_1m", "vol_3m", "amihud")
  priced <- master |>
    dplyr::filter(.data$has_price_source) |>
    dplyr::distinct(.data$instrument_id, .keep_all = TRUE) |>
    dplyr::select("instrument_id", "ticker")
  at_t <- features |>
    dplyr::inner_join(
      dplyr::transmute(formations,
        formation_date = .data$formation_date,
        month = lubridate::floor_date(.data$formation_date, "month")
      ),
      by = dplyr::join_by("month"), relationship = "many-to-one"
    ) |>
    dplyr::select(-"month")

  candidates <- breadth |>
    dplyr::left_join(priced, by = dplyr::join_by("instrument_id"), relationship = "many-to-one") |>
    dplyr::left_join(at_t,
      by = dplyr::join_by("formation_date", "instrument_id"), relationship = "one-to-one"
    ) |>
    dplyr::left_join(dplyr::select(terminal, "instrument_id", "first_date"),
      by = dplyr::join_by("instrument_id"), relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      complete = dplyr::if_all(dplyr::all_of(feature_cols), \(x) !is.na(x)),
      excluded_reason = dplyr::case_when(
        is.na(.data$ticker) ~ "no_price_source",
        .data$n_curr == 0 ~ "identity_unverifiable",
        .data$n_match <= .data$n_curr / 2 ~ "identity_rejected",
        ## A CUSIP change moving holders to a new instrument shows as mass initiations
        .data$n_curr >= min_holders_break & .data$n_init > max_init_share * .data$n_curr ~
          "cusip_change_suspect",
        is.na(.data$date) | .data$date != .data$formation_date ~ "not_traded_at_t",
        .data$close_prev < min_price ~ "price_below_min",
        .data$presence < min_presence ~ "low_presence",
        .data$first_date > .data$formation_date - min_history_days ~ "short_history",
        !.data$complete ~ "missing_features"
      )
    )

  # Liquidity rank among the eligible, then the zero-holder exclusion
  candidates |>
    dplyr::mutate(
      liquidity_rank = rank_desc_with_ties(
        .data$median_volfin_3m, .data$instrument_id, is.na(.data$excluded_reason)
      ),
      .by = "formation_date"
    ) |>
    dplyr::mutate(
      excluded_reason = dplyr::case_when(
        !is.na(.data$excluded_reason) ~ .data$excluded_reason,
        .data$liquidity_rank > n_universe ~ "below_liquidity_rank",
        .data$n_prev == 0 | .data$n_curr == 0 ~ "zero_holders",
        .default = NA_character_
      ),
      in_universe = is.na(.data$excluded_reason)
    ) |>
    dplyr::select(-"complete") |>
    dplyr::arrange(.data$formation_date, .data$instrument_id)
}
