#' Breadth counts recomputed in dplyr, as a reference for `sql_gold_breadth()`
#'
#' An independent, row-by-row restatement of the breadth definition (memo Section 3,
#' Chen, Hong and Stein 2002, p. 180 [src]) used by the gold suite and the unit tests to
#' check the SQL on a sample. It works on in-memory subsets, so it is written for
#' clarity, not speed.
#'
#' @param state Tibble of `silver_holdings_state` rows (`cik`, `cusip`,
#'   `period_of_report`, `valid_from`, `valid_to`, `shares`, `value_usd` in USD).
#' @param formations Tibble from `build_formation_dates()`.
#' @param cohort Tibble with `formation_date`, `cik` and `in_cohort` (lgl).
#' @param master Tibble pinned as `silver_security_master` (`cusip`, `instrument_id`).
#' @param prices_d Tibble of `silver_prices_d` rows (`instrument_id`, `date`, `close`,
#'   raw USD close).
#'
#' @return A tibble with one row per `formation_date` and `instrument_id` held by the
#'   cohort in q-1 or q: `n_prev`, `n_curr`, `n_init`, `n_exit` and `n_match` (quarter-q
#'   holders whose value per share is within 0.5 to 2 times the last close on or before
#'   quarter end, at most 7 days stale).
#'
#' @details
#' Steps, each a plain verb: keep the versions current at t (`valid_from <= t <
#' valid_to`), keep cohort filers, map CUSIPs to instruments, reduce to one row per
#' filer and instrument (a filer holding two CUSIPs of one instrument counts once), then
#' count. A position with zero shares is not a holding.
#' @noRd
reference_gold_breadth <- function(state, formations, cohort, master, prices_d) {
  # Report quarters read at each formation, tagged q (current) or q-1
  quarters <- dplyr::bind_rows(
    dplyr::transmute(formations, .data$formation_date, .data$report_quarter,
      period_of_report = .data$report_quarter, is_curr = TRUE
    ),
    dplyr::transmute(formations, .data$formation_date, .data$report_quarter,
      period_of_report = .data$prev_quarter, is_curr = FALSE
    )
  )
  map <- dplyr::distinct(dplyr::select(master, "cusip", "instrument_id"))

  # One row per formation, instrument and cohort filer
  holders <- state |>
    dplyr::filter(.data$shares > 0) |>
    dplyr::inner_join(quarters,
      by = dplyr::join_by("period_of_report"), relationship = "many-to-many"
    ) |>
    dplyr::filter(
      .data$valid_from <= .data$formation_date,
      is.na(.data$valid_to) | .data$valid_to > .data$formation_date
    ) |>
    dplyr::semi_join(dplyr::filter(cohort, .data$in_cohort),
      by = dplyr::join_by("formation_date", "cik")
    ) |>
    dplyr::inner_join(map, by = dplyr::join_by("cusip"), relationship = "many-to-many") |>
    dplyr::summarise(
      in_curr = any(.data$is_curr), in_prev = any(!.data$is_curr),
      value = sum(.data$value_usd[.data$is_curr]), shares = sum(.data$shares[.data$is_curr]),
      .by = c("formation_date", "report_quarter", "instrument_id", "cik")
    )

  # Quarter-end close for the identity check
  quarter_close <- holders |>
    dplyr::distinct(.data$instrument_id, .data$report_quarter) |>
    dplyr::left_join(dplyr::select(prices_d, "instrument_id", "date", "close"),
      by = dplyr::join_by("instrument_id", closest(report_quarter >= date)),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(.data$date >= .data$report_quarter - 7) |>
    dplyr::select("instrument_id", "report_quarter", "close")

  holders |>
    dplyr::left_join(quarter_close,
      by = dplyr::join_by("instrument_id", "report_quarter"), relationship = "many-to-one"
    ) |>
    dplyr::mutate(ratio = .data$value / .data$shares / .data$close) |>
    dplyr::summarise(
      n_prev = sum(.data$in_prev), n_curr = sum(.data$in_curr),
      n_init = sum(.data$in_curr & !.data$in_prev), n_exit = sum(.data$in_prev & !.data$in_curr),
      n_match = sum(.data$in_curr & dplyr::coalesce(dplyr::between(.data$ratio, 0.5, 2), FALSE)),
      .by = c("formation_date", "instrument_id")
    ) |>
    dplyr::arrange(.data$formation_date, .data$instrument_id)
}
