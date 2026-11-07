#' Read a sample-window end date recorded in a one-line text file
#'
#' The Tiingo cache and the pipeline manifest each record the `window_end` they were
#' built for in a file holding one `YYYY-MM-DD` line (`write_window_end()`).
#'
#' @param path Character scalar, path of the record.
#'
#' @return Date scalar; `NA` when the file does not exist (nothing recorded yet).
#' @noRd
read_window_end <- function(path) {
  # Validate inputs
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("path must be a single file path.")
  }

  # Read
  if (!file.exists(path)) {
    return(as.Date(NA))
  }
  ## A damaged record must stop: read as "no record", it would let a stale cache pass
  value <- readLines(path, warn = FALSE)
  out <- if (length(value) == 1) lubridate::ymd(value, quiet = TRUE) else as.Date(NA)
  if (is.na(out)) stop(path, " must hold one date in YYYY-MM-DD format.")
  out
}
