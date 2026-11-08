#' Build the formation dates of the study and the report quarters each one reads
#'
#' Formation date t is the last trading day of February, May, August and November
#' (plan Section 1.2). The signal at t compares the two consecutive report quarters
#' whose filing deadline (quarter end plus 45 days) fell in or before the formation
#' month: Q4 for end-February, Q1 for end-May, Q2 for end-August, Q3 for end-November.
#'
#' @param calendar Date vector, trading days (silver trading calendar), sorted.
#' @param first_quarter Date scalar, earliest report quarter that may serve as q-1
#'   (2013-06-30: the XML data sets start in May 2013, so 2013Q1 is partial).
#'
#' @return A tibble with one row per formation, ordered by date: `formation_date`
#'   (Date, last trading day of the month), `report_quarter` (q, Date quarter end),
#'   `prev_quarter` (q-1) and `label_date` (Date, first day of the next month: the
#'   `dates` value of every gold panel row built at this formation).
#'
#' @details
#' The label is the first day of the holding month: a row dated 2020-09-01 carries what
#' was known at the close of the last trading day of August 2020, and its forward
#' return is September's. Every label is therefore later than its information.
#' @noRd
build_formation_dates <- function(calendar, first_quarter = as.Date("2013-06-30")) {
  # Validate inputs
  if (!inherits(calendar, "Date") || length(calendar) == 0 || anyNA(calendar)) {
    stop("calendar must be a non-empty Date vector without NA.")
  }

  # Last trading day of each formation month
  formations <- tibble::tibble(date = calendar) |>
    dplyr::mutate(month = lubridate::floor_date(.data$date, "month")) |>
    dplyr::filter(lubridate::month(.data$date) %in% c(2, 5, 8, 11)) |>
    dplyr::summarise(formation_date = max(.data$date), .by = "month")

  # Report quarter whose deadline fell in the formation month: the quarter ending two
  # months before (end-February reads the quarter ended in December)
  shift <- function(month, n) lubridate::add_with_rollback(month, lubridate::period(n, "month"))
  quarter_end <- function(month) lubridate::ceiling_date(month, "month") - 1
  formations |>
    dplyr::mutate(
      report_quarter = quarter_end(shift(.data$month, -2)),
      prev_quarter = quarter_end(shift(.data$month, -5)),
      label_date = shift(.data$month, 1)
    ) |>
    ## The calendar's last month may be unfinished, so its last day is not a formation
    dplyr::filter(
      .data$prev_quarter >= first_quarter,
      .data$month < lubridate::floor_date(max(calendar), "month")
    ) |>
    dplyr::select("formation_date", "report_quarter", "prev_quarter", "label_date") |>
    dplyr::arrange(.data$formation_date)
}
