#' Compare the bulk filing dates and usable dates with EDGAR acceptance times
#'
#' The point-in-time rule of the whole study is that a filing enters the backtest on its
#' `usable_date`, the first trading day strictly after its bulk `FILING_DATE`, and only
#' at formations on or after that day. This is safe if `FILING_DATE` is never earlier
#' than the moment EDGAR made the filing public. This function checks that directly.
#'
#' @param checked Tibble from `download_filing_index_dates()`: `accession_number`,
#'   `filing_date`, `usable_date`, `edgar_filing_date` and `edgar_accepted`.
#'
#' @return A list: `rows` (`checked` with `accepted_date`, `same_filing_date`,
#'   `accepted_by_filing_date`, `usable_after_acceptance` and `public_before_trade`) and
#'   `summary` (tibble with `n_sampled`, `n_read` and the share of read filings passing
#'   each check).
#'
#' @details
#' A submission accepted after 5:30 pm Eastern is deemed filed the next business day
#' (Regulation S-T Rule 13(a)(2)), so `accepted_by_filing_date` allows the acceptance
#' day to be earlier than the filing date but never later. `usable_after_acceptance`
#' requires the usable date to fall strictly after the acceptance day: the portfolio
#' trades at the close of a day on which the filing was already public.
#'
#' EDGAR can grant a filing date earlier than the acceptance time (a filing date
#' adjustment), so `accepted_by_filing_date` and `usable_after_acceptance` can fail
#' without any look-ahead. The binding condition is `public_before_trade`: the filing was
#' accepted before 4 pm Eastern on its usable date, the close at which a portfolio formed
#' that day trades.
#' @noRd
audit_filing_dates <- function(checked) {
  rows <- checked |>
    dplyr::mutate(
      accepted_date = as.Date(substr(.data$edgar_accepted, 1, 10)),
      same_filing_date = .data$edgar_filing_date == .data$filing_date,
      accepted_by_filing_date = .data$accepted_date <= .data$filing_date,
      usable_after_acceptance = .data$usable_date > .data$accepted_date,
      ## Both strings are Eastern time 'YYYY-MM-DD HH:MM:SS', so they compare as text
      public_before_trade = .data$edgar_accepted < paste(.data$usable_date, "16:00:00")
    )
  read <- dplyr::filter(rows, !is.na(.data$accepted_date))
  list(
    rows = rows,
    summary = tibble::tibble(
      n_sampled = nrow(rows), n_read = nrow(read),
      share_same_filing_date = mean(read$same_filing_date),
      share_accepted_by_filing_date = mean(read$accepted_by_filing_date),
      share_usable_after_acceptance = mean(read$usable_after_acceptance),
      share_public_before_trade = mean(read$public_before_trade)
    )
  )
}
