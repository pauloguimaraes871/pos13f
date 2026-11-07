#' Download one SEC Form 13F archive to the local cache
#'
#' Side effect: writes the zip into `dest_dir`. A file already cached with the expected
#' size is reused, so reruns cost no request.
#'
#' @param url Character scalar, absolute url of the archive (from [list_sec_archives()]).
#' @param dest_dir Character scalar, cache folder; created if missing.
#' @param user_agent Character scalar, `"Name email"`, required by SEC for automated
#'   requests (read from `SEC_USER_AGENT`).
#' @param expected_bytes Numeric scalar, size announced by SEC (`NA` when unknown: then
#'   any existing file is reused and the download is not size-checked).
#' @param force Logical; download even when a cached file of the expected size exists.
#'   [deploy_bronze()] sets it when SEC's `Last-Modified` date shows a republication.
#' @param verbose Logical; print a line per download.
#'
#' @return Character scalar, path of the cached zip.
#'
#' @details
#' Requests are throttled to 5 per second (SEC allows 10) and retried up to 3 times on
#' transient errors. The file is downloaded to a temporary name, its size is compared
#' with `expected_bytes`, and only a complete file is renamed into the cache: an
#' interrupted or truncated transfer never looks cached.
#' @export
download_sec_archive <- function(url, dest_dir, user_agent = Sys.getenv("SEC_USER_AGENT"),
                                 expected_bytes = NA_real_, force = FALSE, verbose = TRUE) {
  # Validate inputs
  if (!is.character(url) || length(url) != 1 || !nzchar(url)) {
    stop("url must be a single non-empty string (archive url).")
  }
  if (!is.character(user_agent) || length(user_agent) != 1 || !nzchar(user_agent)) {
    stop("user_agent must be set (SEC_USER_AGENT): SEC rejects anonymous requests.")
  }
  bytes_ok <- is.numeric(expected_bytes) && length(expected_bytes) == 1 &&
    (is.na(expected_bytes) || expected_bytes > 0)
  if (!bytes_ok) {
    stop("expected_bytes must be a positive number or NA.")
  }

  # Reuse a complete cached file
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, basename(url))
  size_matches <- is.na(expected_bytes) || isTRUE(file.size(dest) == expected_bytes)
  if (!force && file.exists(dest) && size_matches) {
    return(dest)
  }

  # Download to a temporary name, verify, then move into the cache
  tmp <- paste0(dest, ".part")
  on.exit(unlink(tmp), add = TRUE)
  ## The body (at most about 100 MB) is written explicitly, which also works with the
  ## mocked responses used in unit tests
  resp <- httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_throttle(rate = 5, realm = "sec.gov") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  writeBin(httr2::resp_body_raw(resp), tmp)
  ## A short transfer is rejected here, before it can be mistaken for the archive
  got <- file.size(tmp)
  if (!is.na(expected_bytes) && !isTRUE(got == expected_bytes)) {
    stop(basename(url), ": downloaded ", got, " bytes, SEC announced ", expected_bytes,
      ". The file was not cached.",
      call. = FALSE
    )
  }
  if (file.exists(dest)) unlink(dest)
  if (!file.rename(tmp, dest)) {
    stop("Could not move the download into the cache: ", dest, call. = FALSE)
  }
  if (verbose) message("Downloaded ", basename(dest), " (", round(file.size(dest) / 1e6), " MB)")
  dest
}
