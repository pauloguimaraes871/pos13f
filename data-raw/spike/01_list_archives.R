# G1 spike, step 1a: list the SEC Form 13F data set archives and their sizes
#
# Throwaway spike script (plan Section 3), not package code.
# Question answered: how many archives exist and how much disk the downloads need.
# Output: data-raw/cache/spike/archives.csv (one row per archive: file, size_mb, url)
# Run from the repository root (D:/dev/pos13f).

# Setup
## SEC requires every automated request to carry a User-Agent with a name and an email
## (read from .Renviron, never printed). Without it SEC answers 403 Forbidden.
sec_ua <- Sys.getenv("SEC_USER_AGENT")
if (!nzchar(sec_ua)) stop("SEC_USER_AGENT is not set; see .Renviron.example.")
out_dir <- "data-raw/cache/spike"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## One helper builds every SEC request the same way:
### req_user_agent: attaches the User-Agent header
### req_throttle:   at most 5 requests per second (SEC allows 10; we keep headroom)
### req_retry:      retries up to 3 times on temporary errors such as a timeout
sec_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(sec_ua) |>
    httr2::req_throttle(rate = 5, realm = "sec.gov") |>
    httr2::req_retry(max_tries = 3)
}

# Archive links from the data set page
## Download the HTML of the page and pick every link ending in form13f...zip
page_url <- "https://www.sec.gov/data-research/sec-markets-data/form-13f-data-sets"
page_html <- sec_request(page_url) |>
  httr2::req_perform() |>
  httr2::resp_body_string()
hrefs <- stringr::str_match_all(page_html, "href=\"([^\"]+form13f[^\"]*\\.zip)\"")[[1]][, 2]

## Links on the page are relative ("/files/..."), so prefix the SEC domain
archives <- tibble::tibble(href = unique(hrefs)) |>
  dplyr::mutate(
    url = dplyr::if_else(
      stringr::str_starts(.data$href, "http"), .data$href,
      paste0("https://www.sec.gov", .data$href)
    ),
    file = basename(.data$url)
  )

# File sizes without downloading the files
## A HEAD request asks the server only for the file's description (its "headers"),
## not for the file itself. The Content-Length header is the size in bytes. This gives
## the total download size in a few seconds instead of downloading 3 GB.
head_size <- function(url) {
  resp <- sec_request(url) |>
    httr2::req_method("HEAD") |>
    httr2::req_perform()
  as.numeric(httr2::resp_header(resp, "content-length"))
}
archives <- archives |>
  dplyr::mutate(size_mb = purrr::map_dbl(.data$url, head_size) / 1e6) |>
  dplyr::select("file", "size_mb", "url")

# Save and report
readr::write_csv(archives, file.path(out_dir, "archives.csv"))
print(archives |> dplyr::select("file", "size_mb"), n = Inf)
cat("archives:", nrow(archives), "| total compressed GB:",
    round(sum(archives$size_mb) / 1e3, 2), "\n")
