#' List the SEC Form 13F data set archives with their sizes
#'
#' Fetches the SEC data set page, parses the archive links with
#' `parse_sec_archive_links()` and asks the server for each file's size with a HEAD
#' request (headers only, no download). The size lets the bronze layer prove that every
#' archive was downloaded completely.
#'
#' @param user_agent Character scalar, `"Name email"` required by SEC.
#' @param verbose Logical; print the number of archives found.
#'
#' @return A tibble with the columns of `parse_sec_archive_links()` plus `size_bytes`
#'   (double) and `last_modified` (character, SEC's `Last-Modified` header).
#' @export
list_sec_archives <- function(user_agent = Sys.getenv("SEC_USER_AGENT"), verbose = TRUE) {
  # Validate inputs
  if (!is.character(user_agent) || length(user_agent) != 1 || !nzchar(user_agent)) {
    stop("user_agent must be set (SEC_USER_AGENT): SEC rejects anonymous requests.")
  }

  # Requests share throttling (5 per second, SEC allows 10) and retries
  sec_request <- function(url) {
    httr2::request(url) |>
      httr2::req_user_agent(user_agent) |>
      httr2::req_throttle(rate = 5, realm = "sec.gov") |>
      httr2::req_retry(max_tries = 3, retry_on_failure = TRUE)
  }

  # Parse the page, then ask each file's size
  page_url <- "https://www.sec.gov/data-research/sec-markets-data/form-13f-data-sets"
  archives <- sec_request(page_url) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    parse_sec_archive_links()
  ## Size proves completeness; Last-Modified identifies a republication that keeps the
  ## same size, so bronze re-downloads when SEC revises an archive
  head_info <- function(url) {
    resp <- sec_request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_perform()
    tibble::tibble(
      size_bytes = as.numeric(httr2::resp_header(resp, "content-length")),
      last_modified = httr2::resp_header(resp, "last-modified", default = NA_character_)
    )
  }
  archives <- dplyr::bind_cols(archives, purrr::map(archives$url, head_info) |> purrr::list_rbind())
  total_gb <- round(sum(archives$size_bytes) / 1e9, 2)
  if (verbose) message(nrow(archives), " archives, ", total_gb, " GB")
  archives
}
