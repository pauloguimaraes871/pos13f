#' Row counts of pins uploaded as parquet files
#'
#' Large tables are pinned as parquet files rather than data frames, so their pin
#' metadata has no row count; the count is read from each file's parquet metadata,
#' without loading the data.
#'
#' @param board A `pins_board`.
#' @param pins Character vector, names of file pins.
#'
#' @return A tibble with `pin` and `n_rows` (summed over the files of the pin).
#' @noRd
report_pin_rows <- function(board, pins) {
  tibble::tibble(
    pin = pins,
    n_rows = purrr::map_dbl(pins, function(pin) {
      files <- pins::pin_download(board, pin)
      sum(purrr::map_dbl(files, \(f) as.numeric(nanoparquet::parquet_info(f)$num_rows)))
    })
  )
}
