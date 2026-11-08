#' Download the monthly Fama-French five factors and momentum
#'
#' Reads the US research factors from Kenneth French's data library through
#' `frenchdata`: the five factors of Fama and French (2015) and the momentum factor
#' (UMD), monthly. They are the benchmark of the F1 alpha regressions (memo Section 8).
#'
#' @param verbose Logical; print download messages.
#'
#' @return A tibble with one row per month: `dates` (Date, first day of the month, the
#'   convention of the gold panels) and `mkt_rf`, `smb`, `hml`, `rmw`, `cma`, `rf`, `umd`
#'   (percentage points per month, as published).
#'
#' @details
#' The library publishes months as `YYYYMM`; each is mapped to its first day, so a
#' factor row and a gold panel row dated alike cover the same calendar month.
#' @noRd
download_ff_factors <- function(verbose = TRUE) {
  monthly <- function(dataset) {
    data <- frenchdata::download_french_data(dataset)
    ## The first subset is the monthly table; annual tables follow
    tibble::as_tibble(data$subsets$data[[1]]) |>
      dplyr::mutate(dates = lubridate::ymd(paste0(.data$date, "01"))) |>
      dplyr::select(-"date")
  }
  if (verbose) message("Downloading Fama-French five factors and momentum")
  five <- monthly("Fama/French 5 Factors (2x3)")
  momentum <- monthly("Momentum Factor (Mom)")
  five |>
    dplyr::inner_join(momentum, by = dplyr::join_by("dates"), relationship = "one-to-one") |>
    dplyr::transmute(
      dates = .data$dates, mkt_rf = .data$`Mkt-RF`, smb = .data$SMB, hml = .data$HML,
      rmw = .data$RMW, cma = .data$CMA, rf = .data$RF, umd = as.numeric(.data$Mom)
    ) |>
    dplyr::arrange(.data$dates)
}
