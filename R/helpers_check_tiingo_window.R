#' Check that the Tiingo price cache can serve a sample window
#'
#' Tiingo restates the whole adjusted-price history of a ticker after every split or
#' dividend, so prices cached for one window cannot be extended with prices downloaded
#' later: the two parts would carry different adjustments. A window that ends later than
#' the one the cache was downloaded for needs a full redownload, which the user starts
#' by deleting the cached price files (it takes about two hours).
#'
#' @param window_end Date scalar, last day of the SEC filing window of this run (set
#'   by the user in `_targets.R`).
#' @param n_cached Numeric scalar, number of cached price files (`t_*.parquet`).
#' @param cache_end Date scalar, the window recorded in the cache folder when its first
#'   ticker was downloaded; `NA` when there is no record.
#' @param manifest_end Date scalar, the window of the last concluded bronze layer, from
#'   the pipeline manifest; `NA` when there is no manifest.
#'
#' @return Date scalar, the window the cache holds after this run: `window_end` for an
#'   empty cache, otherwise the recorded window. Stops when the cache cannot serve
#'   `window_end`.
#'
#' @details
#' The cache's own record takes precedence over the manifest: the manifest is written
#' only when bronze concludes, so after an interrupted download it still names the
#' previous window. The manifest is the fallback for a cache filled before the cache
#' recorded its window (the first full download, 2026-11-07).
#' @noRd
check_tiingo_window <- function(window_end, n_cached, cache_end = as.Date(NA),
                                manifest_end = as.Date(NA)) {
  # Validate inputs
  if (!inherits(window_end, "Date") || length(window_end) != 1 || is.na(window_end)) {
    stop("window_end must be a single non-missing Date (set it in _targets.R).")
  }
  if (!is.numeric(n_cached) || length(n_cached) != 1 || is.na(n_cached) || n_cached < 0) {
    stop("n_cached must be a non-negative count of cached price files.")
  }
  for (arg in list(cache_end, manifest_end)) {
    if (!inherits(arg, "Date") || length(arg) != 1) {
      stop("cache_end and manifest_end must be single Dates (NA when not recorded).")
    }
  }

  # Decide
  ## Empty cache: every ticker is downloaded in this run, so it holds this window
  if (n_cached == 0) {
    return(window_end)
  }
  cached_end <- if (!is.na(cache_end)) cache_end else manifest_end
  hint <- paste(
    "Delete the t_*.parquet files in the Tiingo cache folder to redownload every",
    "ticker (about two hours)."
  )
  if (is.na(cached_end)) {
    stop("The Tiingo cache holds prices of an unknown window (no record in the cache ",
      "folder or the manifest). ", hint,
      call. = FALSE
    )
  }
  ## A shorter or equal window only reads part of prices of one vintage: consistent
  if (window_end > cached_end) {
    stop("window_end (", window_end, ") is later than the window the Tiingo cache was ",
      "downloaded for (", cached_end, "). Cached adjusted prices cannot be extended: ",
      "Tiingo restates them after splits and dividends. ", hint,
      call. = FALSE
    )
  }
  cached_end
}
