#' Gold suite block: the universe screens did what the plan says
#'
#' Called by [run_test_gold()]. Follows the pipeline's silver filter tests ("nano caps
#' are indeed excluded", "ids to exclude were correctly removed"): every exclusion reason
#' is recomputed independently (`expect_universe_reasons()`), no member breaks a screen,
#' the liquidity cut is a clean boundary, the panels hold exactly the members, and
#' membership moves slowly between formations.
#'
#' @param panels Named list from `build_gold_panels()` (uses `signals_m_df` and
#'   `universe_diagnostics`).
#' @param n_universe,min_price,min_history_days Numeric scalars, the universe screens.
#' @param max_membership_turnover Numeric scalar, largest share of a formation's members
#'   that were not members at the previous formation (observed 2026-11-08: at most 0.08).
#' @param max_reentry_share Numeric scalar, largest share of member rows that return after
#'   missing exactly one formation (observed: 0.014, mostly at the liquidity cut).
#'
#' @return `NULL`, invisibly; called for its `testthat` expectations.
#' @noRd
check_gold_universe_filters <- function(panels, n_universe, min_price, min_history_days,
                                        max_membership_turnover, max_reentry_share) {
  universe <- panels$universe_diagnostics
  members <- dplyr::filter(universe, .data$in_universe)

  testthat::test_that("every exclusion reason matches an independent recomputation", {
    expected <- expect_universe_reasons(universe, n_universe, min_price, min_history_days)
    compared <- universe |>
      dplyr::select("formation_date", "instrument_id", "excluded_reason", "liquidity_rank") |>
      dplyr::inner_join(expected,
        by = dplyr::join_by("formation_date", "instrument_id"), relationship = "one-to-one"
      )
    testthat::expect_equal(nrow(compared), nrow(universe))
    testthat::expect_identical(compared$excluded_reason, compared$expected_reason)
    ranked <- dplyr::filter(compared, !is.na(.data$expected_rank))
    testthat::expect_equal(as.numeric(ranked$liquidity_rank), as.numeric(ranked$expected_rank))
  })

  testthat::test_that("no universe member breaks a screen", {
    testthat::expect_true(all(members$close_prev >= min_price))
    testthat::expect_true(all(members$date == members$formation_date))
    testthat::expect_true(all(members$formation_date - members$first_date >= min_history_days))
    testthat::expect_true(all(2 * members$n_match > members$n_curr))
    testthat::expect_true(all(members$n_prev > 0 & members$n_curr > 0))
    testthat::expect_true(all(members$liquidity_rank <= n_universe))
    testthat::expect_false(anyNA(members$ticker))
  })

  testthat::test_that("the liquidity cut is a clean boundary in every formation", {
    boundary <- universe |>
      dplyr::filter(!is.na(.data$liquidity_rank)) |>
      dplyr::summarise(
        least_liquid_in = min(.data$median_volfin_3m[.data$liquidity_rank <= n_universe]),
        most_liquid_out = max(c(-Inf, .data$median_volfin_3m[.data$liquidity_rank > n_universe])),
        .by = "formation_date"
      )
    testthat::expect_true(all(boundary$least_liquid_in >= boundary$most_liquid_out))
  })

  testthat::test_that("panels hold exactly the members of each formation", {
    ## The formation label of a panel row is its holding month minus the signal's age
    in_panel <- panels$signals_m_df |>
      dplyr::transmute(
        instrument_id = .data$tickers,
        label_date = formation_label_of(.data$dates, .data$signal_age_months)
      ) |>
      dplyr::distinct()
    expected <- dplyr::distinct(dplyr::select(members, "instrument_id", "label_date"))
    key <- dplyr::join_by("instrument_id", "label_date")
    testthat::expect_equal(nrow(dplyr::anti_join(in_panel, expected, by = key)), 0)
    testthat::expect_equal(nrow(dplyr::anti_join(expected, in_panel, by = key)), 0)
  })

  testthat::test_that("membership moves slowly between formations", {
    formations <- sort(unique(members$formation_date))
    k <- dplyr::transmute(members,
      instrument_id = .data$instrument_id, k = match(.data$formation_date, formations)
    )
    entered <- dplyr::anti_join(k, dplyr::mutate(k, k = .data$k + 1),
      by = dplyr::join_by("instrument_id", "k")
    )
    turnover <- dplyr::summarise(entered, n_new = dplyr::n(), .by = "k") |>
      dplyr::inner_join(dplyr::summarise(k, n = dplyr::n(), .by = "k"),
        by = dplyr::join_by("k"), relationship = "one-to-one"
      ) |>
      dplyr::filter(.data$k > min(.env$k$k))
    testthat::expect_true(all(turnover$n_new / turnover$n <= max_membership_turnover))
    ## A member that skips one formation and returns: flicker at a screen
    reentries <- dplyr::semi_join(entered, dplyr::mutate(k, k = .data$k + 2),
      by = dplyr::join_by("instrument_id", "k")
    )
    testthat::expect_lte(nrow(reentries) / nrow(k), max_reentry_share)
  })
  invisible(NULL)
}
