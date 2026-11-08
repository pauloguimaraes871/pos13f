#' Deploy the filing-date audit: a random sample of filings checked against EDGAR
#'
#' Draws filings at random (fixed seed) from `silver_filings`, originals and amendments
#' from every quarter, reads their EDGAR index pages and pins the comparison under
#' `silver_filing_date_audit`. It stops if any read filing was accepted at or after 4 pm
#' Eastern on its usable date, which would mean a portfolio formed at that close could
#' use a filing before it was public.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param n Numeric scalar, filings sampled.
#' @param seed Integer scalar, the random seed.
#' @param verbose Logical; print progress and pins messages.
#'
#' @return Character scalar, the version of `silver_filing_date_audit`.
#' @export
deploy_filing_date_audit <- function(board, n = 300, seed = 20261108, verbose = TRUE) {
  # Validate inputs
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")

  filings <- pins::pin_read(board, "silver_filings")
  sample <- withr::with_seed(seed, dplyr::slice_sample(filings, n = min(n, nrow(filings)))) |>
    dplyr::select(
      "accession_number", "cik", "period_of_report", "submission_type",
      "is_amendment", "filing_date", "usable_date"
    )
  if (verbose) message("Filing-date audit: reading ", nrow(sample), " EDGAR index pages")
  audit <- audit_filing_dates(download_filing_index_dates(sample))
  late <- dplyr::filter(audit$rows, !.data$public_before_trade)
  if (nrow(late) > 0) {
    stop(
      "Sampled filings accepted after the close of their usable_date: ",
      paste(late$accession_number, collapse = ", "), "."
    )
  }
  if (audit$summary$n_read < 0.9 * audit$summary$n_sampled) {
    stop("Fewer than 90% of the sampled EDGAR pages could be read; rerun later.")
  }
  inputs <- list(silver_filings = pins::pin_meta(board, "silver_filings")$local$version)
  save_local_pin(board, audit$rows, "silver", "filing_date_audit",
    inputs = inputs, extra = as.list(audit$summary), verbose = verbose
  )
}
