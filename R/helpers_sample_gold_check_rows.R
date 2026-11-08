#' Draw the panel rows the gold suite recomputes by hand
#'
#' A stratified random sample of finished panel rows, drawn with a fixed seed so the
#' check is reproducible, and drawn by the computer so nobody picks convenient rows.
#' Strata follow the pipeline's "regular stock, delisted stock" manual checks:
#' `regular` (any row), `terminal_month` (the month of an instrument's last price, where
#' the stress return applies), `first_formation` (the earliest panel month, closest to
#' the start of the price history), `few_holders` (bottom decile of quarter-q holders,
#' where one filer moves F1 most) and `old_signal` (third month of a frozen signal).
#'
#' @param panels Named list from `build_gold_panels()` (uses `signals_m_df` and
#'   `fwd_1m_returns_m_df`).
#' @param terminal Tibble pinned as `silver_terminal_events` (`instrument_id`,
#'   `last_date`, `is_live`).
#' @param n_per_stratum Numeric scalar, rows drawn per stratum (fewer when a stratum is
#'   smaller).
#' @param seed Integer scalar, the random seed.
#'
#' @return A tibble with one row per sampled panel row: `id`, `tickers`, `dates`,
#'   `signal_age_months`, `stratum` (the first stratum that drew it) and one logical
#'   column per stratum (whether the row belongs to it), with attribute
#'   `available_strata`: the strata that had at least one row to draw.
#' @noRd
sample_gold_check_rows <- function(panels, terminal, n_per_stratum = 8, seed = 20261108) {
  # Validate inputs
  if (!is.numeric(n_per_stratum) || length(n_per_stratum) != 1 || n_per_stratum < 1) {
    stop("n_per_stratum must be a single number of at least 1.")
  }

  # Finished rows with the facts each stratum needs
  ended <- dplyr::filter(terminal, !.data$is_live)
  rows <- panels$signals_m_df |>
    dplyr::select("id", "tickers", "dates", "signal_age_months", "n_holders_curr") |>
    dplyr::semi_join(dplyr::filter(panels$fwd_1m_returns_m_df, !is.na(.data$fwd_return_1m)),
      by = dplyr::join_by("id")
    ) |>
    dplyr::left_join(dplyr::select(ended, tickers = "instrument_id", "last_date"),
      by = dplyr::join_by("tickers"), relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      regular = TRUE,
      terminal_month = dplyr::coalesce(
        lubridate::floor_date(.data$last_date, "month") == .data$dates, FALSE
      ),
      first_formation = .data$dates == min(.data$dates),
      few_holders = .data$n_holders_curr <= stats::quantile(.data$n_holders_curr, 0.1),
      old_signal = .data$signal_age_months == 2
    )

  # Draw each stratum, then keep each row once
  strata <- c("regular", "terminal_month", "first_formation", "few_holders", "old_signal")
  available <- strata[purrr::map_lgl(strata, \(s) any(rows[[s]]))]
  drawn <- withr::with_seed(seed, {
    purrr::map(strata, function(s) {
      pool <- dplyr::filter(rows, .data[[s]])
      dplyr::mutate(dplyr::slice_sample(pool, n = min(n_per_stratum, nrow(pool))), stratum = s)
    })
  }) |>
    dplyr::bind_rows() |>
    dplyr::distinct(.data$id, .keep_all = TRUE) |>
    dplyr::select("id", "tickers", "dates", "signal_age_months", "stratum", dplyr::all_of(strata))
  attr(drawn, "available_strata") <- available
  drawn
}
