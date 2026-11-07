#' First trading day strictly after each filing date
#'
#' The point-in-time rule of the whole study (plan Section 1.2): the SEC data sets carry
#' a filing date but no time stamp, so a filing made after the close is indistinguishable
#' from one made before the open. Its content is therefore usable only from the next
#' trading day, never on the filing date itself.
#'
#' @param filing_date Date vector, filing dates (`FILING_DATE` of SUBMISSION).
#' @param calendar Date vector, the trading days, sorted, unique, without `NA` (SPY
#'   trading dates, `read_trading_calendar()`).
#'
#' @return Date vector of the same length: the first calendar day after each filing
#'   date; `NA` where the filing date is missing or the calendar ends first.
#' @noRd
compute_usable_date <- function(filing_date, calendar) {
  # Validate inputs
  if (!inherits(filing_date, "Date")) stop("filing_date must be a Date vector.")
  valid_calendar <- inherits(calendar, "Date") && length(calendar) > 0 &&
    !anyNA(calendar) && !is.unsorted(calendar, strictly = TRUE)
  if (!valid_calendar) {
    stop("calendar must be a non-empty Date vector, strictly increasing, without NA.")
  }

  # Next trading day
  ## findInterval() counts the calendar days on or before each filing date, so the next
  ## position is the first trading day strictly after it (a trading-day filing waits too)
  position <- findInterval(as.numeric(filing_date), as.numeric(calendar)) + 1L
  calendar[replace(position, position > length(calendar), NA)]
}
