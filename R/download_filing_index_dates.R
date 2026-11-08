#' Download the filing date and acceptance time of 13F filings from EDGAR
#'
#' Reads each filing's EDGAR index page (`<accession>-index.htm`), which shows the
#' filing date and the time EDGAR accepted the submission, Eastern time. Used by the
#' filing-date audit to check the bulk data sets against EDGAR itself.
#'
#' @param filings Tibble with `accession_number` and `cik` (character, zero padded).
#' @param user_agent Character scalar, the SEC-required User-Agent (name and email).
#' @param pause Numeric scalar, seconds between requests (SEC allows at most 10 per second).
#'
#' @return `filings` with `edgar_filing_date` (Date) and `edgar_accepted` (character,
#'   `YYYY-MM-DD HH:MM:SS` Eastern), `NA` when a page cannot be read.
#' @noRd
download_filing_index_dates <- function(filings, user_agent = Sys.getenv("SEC_USER_AGENT"),
                                        pause = 0.25) {
  # Validate inputs
  if (!nzchar(user_agent)) stop("user_agent is empty: set SEC_USER_AGENT (SEC requires it).")

  field <- function(page, label) {
    m <- regmatches(page, regexpr(paste0(label, "</div>\\s*<div class=\"info\">[^<]+"), page))
    if (length(m) == 0) NA_character_ else sub(".*class=\"info\">", "", m)
  }
  one <- function(accession, cik) {
    Sys.sleep(pause)
    url <- sprintf(
      "https://www.sec.gov/Archives/edgar/data/%s/%s/%s-index.htm",
      as.integer(cik), gsub("-", "", accession), accession
    )
    page <- tryCatch(
      httr2::request(url) |>
        httr2::req_user_agent(user_agent) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform() |>
        httr2::resp_body_string(),
      error = function(e) NA_character_
    )
    if (is.na(page)) return(c(NA_character_, NA_character_))
    c(field(page, "Filing Date"), field(page, "Accepted"))
  }
  pages <- purrr::map2(filings$accession_number, filings$cik, one)
  dplyr::mutate(filings,
    edgar_filing_date = as.Date(purrr::map_chr(pages, 1)),
    edgar_accepted = purrr::map_chr(pages, 2)
  )
}
