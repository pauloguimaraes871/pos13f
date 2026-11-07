#' Compute the figures the holdings suite checks, inside duckdb when the tables are lazy
#'
#' The holdings state has about 116 million rows, so the suite cannot read it; it reads
#' these aggregates instead.
#'
#' @param state Table from `build_silver_holdings_state()`.
#' @param positions Table from `summarise_holdings_positions()`.
#' @param sources Table from `build_holdings_sources()`, of the same source as `state`.
#' @param infotable Bronze INFOTABLE rows, of the same source (`ACCESSION_NUMBER`).
#'
#' @return A list with `totals`, a one-row tibble (`n_rows`, `n_duplicate_keys`,
#'   `n_negative`, `n_missing`, `shares_single_source_state`,
#'   `shares_single_source_positions`), and `entry_counts`, a tibble with
#'   `accession_number` and `n_infotable_rows` (all bronze rows of each filing).
#' @noRd
summarise_holdings_checks <- function(state, positions, sources, infotable) {
  keys <- c("cik", "period_of_report", "cusip", "valid_from")
  n_duplicate_keys <- state |>
    dplyr::count(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::collect()
  totals <- state |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_negative = sum(dplyr::if_else(.data$shares < 0 | .data$value_usd < 0, 1, 0)),
      n_missing = sum(dplyr::if_else(
        is.na(.data$shares) | is.na(.data$value_usd) | is.na(.data$valid_from), 1, 0
      ))
    ) |>
    dplyr::collect()

  # Conservation on versions built from one filing: the state must hold exactly that
  # filing's shares, no more (a duplicating join) and no less (a dropping one)
  version_keys <- c("cik", "period_of_report", "valid_from")
  single <- sources |>
    dplyr::mutate(n_filings = dplyr::n(), .by = dplyr::all_of(version_keys)) |>
    dplyr::filter(.data$n_filings == 1)
  from_positions <- positions |>
    dplyr::inner_join(single, by = dplyr::join_by("accession_number")) |>
    dplyr::summarise(shares = sum(.data$shares)) |>
    dplyr::collect()
  from_state <- state |>
    dplyr::semi_join(single, by = version_keys) |>
    dplyr::summarise(shares = sum(.data$shares)) |>
    dplyr::collect()

  list(
    totals = dplyr::mutate(totals,
      n_duplicate_keys = n_duplicate_keys$n,
      shares_single_source_state = from_state$shares,
      shares_single_source_positions = from_positions$shares
    ),
    entry_counts = infotable |>
      dplyr::count(.data$ACCESSION_NUMBER, name = "n_infotable_rows") |>
      dplyr::rename(accession_number = "ACCESSION_NUMBER") |>
      dplyr::collect()
  )
}
