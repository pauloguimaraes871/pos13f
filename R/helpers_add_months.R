#' Add whole months to dates, rolling back to the last day when the day does not exist
#'
#' Gold dates are first days of months, where rollback never applies; the rollback rule
#' keeps the helper safe for quarter ends (31 March plus three months is 30 June).
#'
#' @param x Date vector.
#' @param n Integer scalar, months to add (negative to subtract).
#'
#' @return Date vector of the same length.
#' @noRd
add_months <- function(x, n) {
  if (!inherits(x, "Date")) stop("x must be a Date vector.")
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n != round(n)) {
    stop("n must be a single whole number of months.")
  }
  lubridate::add_with_rollback(x, lubridate::period(n, "month"))
}
