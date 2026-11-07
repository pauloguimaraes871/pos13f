#' Extract the Form 13F archives listed on the SEC data set page
#'
#' Pure parser (no network) so it can be unit-tested; [list_sec_archives()] fetches the
#' page and adds file sizes.
#'
#' @param page_html Character scalar, HTML of
#'   `https://www.sec.gov/data-research/sec-markets-data/form-13f-data-sets`.
#'
#' @return A tibble, one row per archive, ordered by `window_start`, with columns `file`
#'   (zip name), `url` (absolute), `label` (pin-safe id such as `2014q1` or
#'   `01mar2025_31may2025`), `window_start` and `window_end` (Date, the filing-date
#'   window the archive covers).
#'
#' @details
#' Two naming formats exist: `YYYYqN_form13f.zip` (calendar quarter of filing dates,
#' up to 2023) and `DDmonYYYY-DDmonYYYY_form13f.zip` (explicit filing window, from
#' 2024). The window is used by the bronze checks on filing dates.
#' @noRd
parse_sec_archive_links <- function(page_html) {
  # Validate inputs
  if (!is.character(page_html) || length(page_html) != 1) {
    stop("page_html must be a single character string of HTML.")
  }

  # Find the archive links
  hrefs <- stringr::str_match_all(page_html, "href=\"([^\"]+_form13f\\.zip)\"")[[1]][, 2]
  if (length(hrefs) == 0) {
    stop("No Form 13F archive link found on the page: SEC may have changed the page.")
  }

  # Build one row per archive
  tibble::tibble(href = unique(hrefs)) |>
    dplyr::mutate(
      url = dplyr::if_else(stringr::str_starts(.data$href, "http"), .data$href,
        paste0("https://www.sec.gov", .data$href)
      ),
      file = basename(.data$url),
      stem = stringr::str_remove(.data$file, "_form13f\\.zip$"),
      label = stringr::str_replace_all(.data$stem, "-", "_"),
      is_quarter = stringr::str_detect(.data$stem, "^\\d{4}q[1-4]$"),
      ## Quarter archives: first and last day of the calendar quarter
      ## (NA for window archives; quiet because those rows use the other branch)
      quarter_start = lubridate::yq(stringr::str_replace(.data$stem, "q", "."),
        quiet = TRUE
      ),
      ## Window archives: the two dates in the name, such as 01mar2025-31may2025
      window_start = dplyr::if_else(
        .data$is_quarter, .data$quarter_start,
        lubridate::dmy(stringr::str_extract(.data$stem, "^[^-]+"), quiet = TRUE)
      ),
      window_end = dplyr::if_else(
        .data$is_quarter, .data$quarter_start + months(3) - 1,
        lubridate::dmy(stringr::str_extract(.data$stem, "[^-]+$"), quiet = TRUE)
      )
    ) |>
    dplyr::arrange(.data$window_start) |>
    dplyr::select("file", "url", "label", "window_start", "window_end")
}
