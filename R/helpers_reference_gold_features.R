#' Monthly price features recomputed in dplyr, as a reference for the SQL
#'
#' An independent restatement of `sql_gold_monthly_features()` as seen by a panel row:
#' for a target instrument and date, find the instrument's last price day strictly
#' before the date, then compute each feature over explicit trading-day windows that end
#' the day before that price day. Windows are non-equi joins on `day_index`, not SQL
#' window frames, so an error in one implementation does not repeat in the other.
#'
#' @param targets Tibble with `instrument_id` (chr) and `as_of` (Date, the panel row's
#'   `dates`: the first day of the holding month, or the formation label for signals).
#' @param prices_d Tibble of `silver_prices_d` rows (`id`, `instrument_id`, `date`,
#'   `day_index`, `close` in USD, `dollar_volume` in USD).
#' @param returns_d Tibble of `silver_returns_d` rows (`id`, `ret`, a fraction).
#'
#' @return One row per distinct target (`instrument_id`, `as_of`) with `anchor_date`
#'   (the last price day before `as_of`) and the features in the units of the SQL:
#'   `close_prev`, `mean_volfin_3m`, `median_volfin_3m`, `presence`, `daily_vol`,
#'   `vol_3m`, `mom_12_1`, `n_mom`, `rev_1m`, `amihud`.
#'
#' @details
#' Windows, in trading days before the anchor: 63 to 1 (liquidity, presence, `vol_3m`,
#' Amihud), 21 to 1 (`daily_vol`, `rev_1m`), 252 to 22 (momentum). Missing returns are
#' skipped, as SQL aggregates skip NULL; a window without any return gives `NA`. Amihud
#' uses only days with positive dollar volume.
#' @noRd
reference_gold_features <- function(targets, prices_d, returns_d) {
  daily <- prices_d |>
    dplyr::left_join(dplyr::select(returns_d, "id", "ret"),
      by = dplyr::join_by("id"), relationship = "one-to-one"
    ) |>
    dplyr::arrange(.data$instrument_id, .data$date) |>
    dplyr::mutate(close_prev = dplyr::lag(.data$close), .by = "instrument_id") |>
    dplyr::select(
      "instrument_id", "date", "day_index", "close", "dollar_volume", "ret", "close_prev"
    )

  # Anchor: the last price day strictly before the target date
  anchors <- targets |>
    dplyr::distinct(.data$instrument_id, .data$as_of) |>
    dplyr::left_join(
      dplyr::select(daily, "instrument_id",
        anchor_date = "date", anchor_index = "day_index", "close_prev"
      ),
      by = dplyr::join_by("instrument_id", closest(as_of > anchor_date)),
      relationship = "many-to-one"
    )

  # Daily rows in [anchor - from, anchor - to], summarised per target
  over_window <- function(from, to, ...) {
    anchors |>
      dplyr::mutate(lo = .data$anchor_index - from, hi = .data$anchor_index - to) |>
      dplyr::left_join(daily,
        by = dplyr::join_by("instrument_id", between(y$day_index, x$lo, x$hi)),
        relationship = "many-to-many"
      ) |>
      dplyr::summarise(..., .by = c("instrument_id", "as_of"))
  }
  compound <- function(r) if (all(is.na(r))) NA_real_ else 100 * (prod(1 + r, na.rm = TRUE) - 1)
  mean_or_na <- function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
  stdev <- function(r) if (sum(!is.na(r)) < 2) NA_real_ else 100 * stats::sd(r, na.rm = TRUE)
  long <- over_window(63, 1,
    mean_volfin_3m = mean(.data$dollar_volume),
    median_volfin_3m = stats::median(.data$dollar_volume),
    presence = 100 * sum(.data$dollar_volume > 0, na.rm = TRUE) / 63, vol_3m = stdev(.data$ret),
    amihud = 1e6 * mean_or_na((abs(.data$ret) / .data$dollar_volume)[.data$dollar_volume > 0])
  )
  short <- over_window(21, 1, daily_vol = stdev(.data$ret), rev_1m = compound(.data$ret))
  momentum <- over_window(252, 22, mom_12_1 = compound(.data$ret), n_mom = sum(!is.na(.data$ret)))

  key <- dplyr::join_by("instrument_id", "as_of")
  anchors |>
    dplyr::select("instrument_id", "as_of", "anchor_date", "close_prev") |>
    dplyr::left_join(long, by = key, relationship = "one-to-one") |>
    dplyr::left_join(short, by = key, relationship = "one-to-one") |>
    dplyr::left_join(momentum, by = key, relationship = "one-to-one")
}
