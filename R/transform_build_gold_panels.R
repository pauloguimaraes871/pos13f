#' Assemble the gold panels in factoRverse meta_dataframe layout
#'
#' Each formation's universe members stay in the panel for the months until the next
#' formation (three monthly rows), with the formation's signal frozen: quarterly
#' buy-and-hold, never monthly rescreening. Every panel starts with `id`, `tickers`
#' and `dates` (`id = paste0(tickers, "-", dates)`, rows sorted by `id`), where
#' `tickers` is the permanent `instrument_id` and `dates` the first day of the holding
#' month (`build_formation_dates()`).
#'
#' @param formations Tibble from `build_formation_dates()`.
#' @param universe Tibble from `build_gold_universe()`.
#' @param cohort Tibble from `select_gold_cohort()`.
#' @param features Tibble from `sql_gold_monthly_features()`.
#' @param monthly_returns Tibble from `sql_gold_monthly_returns()`.
#' @param terminal Tibble pinned as `silver_terminal_events` (`instrument_id`,
#'   `last_date`, `is_live`).
#' @param last_calendar_day Date scalar, last day of the trading calendar.
#' @param terminal_stress Numeric scalar, return applied on the last price day of an
#'   instrument that stops trading, in the stress column (plan 1.6: -30%).
#'
#' @return A named list of tibbles: `signals_m_df`, `fwd_1m_returns_m_df`,
#'   `liquidity_m_df`, `volatility_m_df` (identical `id`, `tickers`, `dates` columns,
#'   numeric columns after them) and `universe_diagnostics` (one row per formation and
#'   candidate, with dates, ticker and exclusion reason).
#'
#' @details
#' Returns are in percentage points (2 means 2%), as factoRverse expects. The forward
#' return of a row is its holding month's compounded total return. After an
#' instrument's last price the position is closed at that price (baseline: 0 afterwards);
#' the stress column applies `(1 + r) * (1 + terminal_stress) - 1` to the month of the
#' last price. The month containing the calendar's last day is unfinished: its forward
#' returns are `NA`, the only `NA` in the panels (pipeline convention). Liquidity and
#' volatility of a row are the values at the last trading day before its holding month,
#' carried from the latest earlier month when the stock did not trade that month.
#' @noRd
build_gold_panels <- function(formations, universe, cohort, features, monthly_returns,
                              terminal, last_calendar_day, terminal_stress = -0.3) {
  # Rows: universe members, frozen for the months until the next formation
  last_month <- lubridate::floor_date(last_calendar_day, "month")
  holding <- formations |>
    dplyr::mutate(
      next_label = dplyr::lead(.data$label_date, default = add_months(last_month, 1))
    ) |>
    dplyr::reframe(
      label = seq(.data$label_date, add_months(.data$next_label, -1), by = "month"),
      .by = c("formation_date", "label_date")
    ) |>
    dplyr::filter(.data$label <= last_month) |>
    dplyr::mutate(signal_age_months = as.numeric(dplyr::row_number() - 1), .by = "formation_date")
  sizes <- dplyr::summarise(cohort, cohort_size = sum(.data$in_cohort), .by = "formation_date")
  rows <- universe |>
    dplyr::filter(.data$in_universe) |>
    dplyr::inner_join(sizes, by = dplyr::join_by("formation_date"), relationship = "many-to-one") |>
    dplyr::inner_join(dplyr::select(holding, "formation_date", "label", "signal_age_months"),
      by = dplyr::join_by("formation_date"), relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      tickers = .data$instrument_id, dates = .data$label,
      id = paste0(.data$tickers, "-", .data$dates),
      info_month = add_months(.data$label, -1)
    ) |>
    dplyr::arrange(.data$id)

  key <- c("id", "tickers", "dates")
  list(
    signals_m_df = gold_signal_columns(rows, key),
    fwd_1m_returns_m_df = gold_return_columns(
      rows, key, monthly_returns, terminal,
      last_month, terminal_stress
    ),
    liquidity_m_df = gold_as_of_columns(
      rows, key, features,
      c("mean_volfin_3m", "median_volfin_3m", "presence", "close_prev")
    ),
    volatility_m_df = gold_as_of_columns(rows, key, features, c("daily_vol", "vol_3m")),
    universe_diagnostics = universe |>
      dplyr::left_join(dplyr::select(formations, "formation_date", "label_date", "report_quarter"),
        by = dplyr::join_by("formation_date"), relationship = "many-to-one"
      )
  )
}
