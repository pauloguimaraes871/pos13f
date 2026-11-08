#' Formation label of gold panel rows: the holding month minus the signal's age
#'
#' Every row of a formation's holding months carries the formation's label (the first
#' day of its first holding month) once its age is subtracted. Month arithmetic is done
#' on an integer month index, so it is vectorised over both arguments and never rolls a
#' day over (all panel dates are first days of months).
#'
#' @param dates Date vector, first days of holding months (panel `dates`).
#' @param signal_age_months Numeric vector of whole months (0, 1 or 2), recycled to
#'   `dates`.
#'
#' @return Date vector, the first day of the formation's first holding month.
#' @noRd
formation_label_of <- function(dates, signal_age_months) {
  # Validate inputs
  if (!inherits(dates, "Date") || any(lubridate::mday(dates) != 1, na.rm = TRUE)) {
    stop("dates must be a Date vector of first days of months.")
  }
  if (any(signal_age_months %% 1 != 0, na.rm = TRUE)) {
    stop("signal_age_months must hold whole numbers of months.")
  }

  # Month index, shifted, back to a date
  index <- lubridate::year(dates) * 12 + lubridate::month(dates) - 1 - signal_age_months
  lubridate::make_date(index %/% 12, index %% 12 + 1, 1)
}
