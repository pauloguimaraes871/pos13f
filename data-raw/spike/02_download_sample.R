# G1 spike, step 1b: download three sample archives and list their contents
#
# Throwaway spike script. Needs archives.csv from 01_list_archives.R.
# The three archives are chosen to cover the cases that can break the pipeline:
#   2014q1                          early window, VALUE in thousands of USD
#   2023q1                          first filings after 2023-01-03, when VALUE switched
#                                   from thousands to dollars
#   01mar2025-31may2025             recent archive in the new filing-window format
# Output: zip files in data-raw/cache/sec/ (git-ignored)

# Setup
sec_ua <- Sys.getenv("SEC_USER_AGENT")
if (!nzchar(sec_ua)) stop("SEC_USER_AGENT is not set; see .Renviron.example.")
cache_dir <- "data-raw/cache/sec"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
archives <- readr::read_csv("data-raw/cache/spike/archives.csv", show_col_types = FALSE)
sample_files <- c("2014q1_form13f.zip", "2023q1_form13f.zip",
                  "01mar2025-31may2025_form13f.zip")

# Download one archive
## Skipped when the file is already on disk with the size the server announced, so the
## script can be rerun without downloading again. Returns the time taken.
download_one <- function(url, file, size_mb) {
  dest <- file.path(cache_dir, file)
  if (file.exists(dest) && abs(file.size(dest) / 1e6 - size_mb) < 1e-3) {
    return(tibble::tibble(file = file, seconds = 0, cached = TRUE))
  }
  t0 <- Sys.time()
  httr2::request(url) |>
    httr2::req_user_agent(sec_ua) |>
    httr2::req_throttle(rate = 5, realm = "sec.gov") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = dest)
  tibble::tibble(
    file = file,
    seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    cached = FALSE
  )
}

## pmap calls download_one once per row, matching the columns url, file and size_mb to
## the function's arguments of the same name
timings <- archives |>
  dplyr::filter(.data$file %in% sample_files) |>
  dplyr::select("url", "file", "size_mb") |>
  purrr::pmap(download_one) |>
  purrr::list_rbind()
print(timings)

# What is inside each zip (listed without extracting)
## Each archive holds seven tab-separated tables plus the SEC readme and a metadata file
contents <- purrr::map(sample_files, function(f) {
  utils::unzip(file.path(cache_dir, f), list = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(archive = f, size_mb = .data$Length / 1e6) |>
    dplyr::select("archive", file = "Name", "size_mb")
}) |>
  purrr::list_rbind()
print(contents, n = Inf)
print(dplyr::summarise(contents, expanded_mb = sum(.data$size_mb), .by = "archive"))
