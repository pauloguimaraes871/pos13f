#' Record a sample-window end date in a one-line text file
#'
#' Used for the window of the Tiingo cache (written before its first download) and for
#' the pipeline manifest (written when the bronze layer concludes). Read back with
#' `read_window_end()`.
#'
#' @param window_end Date scalar, last day of the SEC filing window.
#' @param path Character scalar, path of the record; its folder is created if needed.
#'
#' @return `path`, invisibly.
#' @noRd
write_window_end <- function(window_end, path) {
  # Validate inputs
  if (!inherits(window_end, "Date") || length(window_end) != 1 || is.na(window_end)) {
    stop("window_end must be a single non-missing Date.")
  }
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("path must be a single file path.")
  }

  # Write
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(format(window_end, "%Y-%m-%d"), path)
  invisible(path)
}
