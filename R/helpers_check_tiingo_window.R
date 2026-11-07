#' Decide whether the Tiingo price cache can be reused for a sample window
#'
#' Tiingo restates the whole adjusted-price history of a ticker after every split or
#' dividend, so prices cached for one window cannot be extended with prices downloaded
#' later: the two parts would carry different adjustments. A window that ends later than
#' the one the cache was pinned for therefore needs a full redownload, which must be
#' asked for explicitly (it takes about two hours).
#'
#' @param window_end Date scalar, last day of the SEC filing window of this run (set
#'   by the user in `_targets.R`).
#' @param previous_end Date scalar or `NULL`, the `window_end` recorded in the current
#'   `bronze_tiingo_prices` pin. `NULL` or `NA` when there is no pin yet, or the pin
#'   predates the record (first pin, 2026-11-07).
#' @param force Logical scalar; `TRUE` discards the cache and redownloads every ticker.
#'
#' @return `"refresh"` (clear the cache and download everything) or `"reuse"` (keep
#'   cached tickers, download only new ones). Stops when the window was extended
#'   without `force = TRUE`.
#' @noRd
check_tiingo_window <- function(window_end, previous_end, force = FALSE) {
  # Validate inputs
  if (!inherits(window_end, "Date") || length(window_end) != 1 || is.na(window_end)) {
    stop("window_end must be a single non-missing Date (set it in _targets.R).")
  }
  if (!is.logical(force) || length(force) != 1 || is.na(force)) {
    stop("force must be TRUE or FALSE.")
  }

  # Decide
  if (force) {
    return("refresh")
  }
  ## No recorded window: nothing to be inconsistent with
  if (is.null(previous_end) || is.na(previous_end)) {
    return("reuse")
  }
  ## A shorter or equal window only reads part of prices of one vintage: consistent
  if (window_end <= previous_end) {
    return("reuse")
  }
  stop(
    "window_end (", window_end, ") is later than the window the Tiingo cache was pinned ",
    "for (", previous_end, "). Cached adjusted prices cannot be extended: Tiingo restates ",
    "them after splits and dividends. Rerun deploy_bronze_tiingo() with force = TRUE to ",
    "redownload every ticker (about two hours)."
  )
}
