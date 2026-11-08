#' Filer activity recomputed in dplyr, as a reference for `sql_gold_filer_activity()`
#'
#' An independent restatement of the cohort's position counts for a few filers: read the
#' holdings versions current at t for q-1 and q, keep positions with shares, and count
#' distinct CUSIPs in each quarter and in both. The gold suite compares the result with
#' the cohort table on a random sample of filers.
#'
#' @param state Tibble of `silver_holdings_state` rows of the sampled filers (`cik`,
#'   `cusip`, `period_of_report`, `valid_from`, `valid_to`, `shares`, `value_usd` in USD).
#' @param targets Tibble with `formation_date`, `cik`, `report_quarter` and
#'   `prev_quarter`, one row per sampled filer and formation.
#'
#' @return `targets` (`formation_date`, `cik`) with `n_prev`, `n_curr`, `n_both` and
#'   `value_curr` (USD, quarter q); 0 when the filer reported no position.
#' @noRd
reference_gold_activity <- function(state, targets) {
  held <- function(quarter_col, label) {
    targets |>
      dplyr::select("formation_date", "cik", period_of_report = dplyr::all_of(quarter_col)) |>
      dplyr::inner_join(state,
        by = dplyr::join_by("cik", "period_of_report"), relationship = "one-to-many"
      ) |>
      dplyr::filter(
        .data$shares > 0, .data$valid_from <= .data$formation_date,
        is.na(.data$valid_to) | .data$valid_to > .data$formation_date
      ) |>
      dplyr::summarise(value = sum(.data$value_usd), .by = c("formation_date", "cik", "cusip")) |>
      dplyr::mutate(quarter = label)
  }
  positions <- dplyr::bind_rows(held("prev_quarter", "prev"), held("report_quarter", "curr"))
  counts <- positions |>
    dplyr::summarise(
      in_prev = any(.data$quarter == "prev"), in_curr = any(.data$quarter == "curr"),
      value = sum(.data$value[.data$quarter == "curr"]),
      .by = c("formation_date", "cik", "cusip")
    ) |>
    dplyr::summarise(
      n_prev = sum(.data$in_prev), n_curr = sum(.data$in_curr),
      n_both = sum(.data$in_prev & .data$in_curr), value_curr = sum(.data$value),
      .by = c("formation_date", "cik")
    )
  targets |>
    dplyr::select("formation_date", "cik") |>
    dplyr::left_join(counts,
      by = dplyr::join_by("formation_date", "cik"), relationship = "one-to-one"
    ) |>
    ## A filer without any position has no count row: it holds zero, not unknown
    dplyr::mutate(dplyr::across(
      c("n_prev", "n_curr", "n_both", "value_curr"), \(x) dplyr::coalesce(as.numeric(x), 0)
    ))
}
