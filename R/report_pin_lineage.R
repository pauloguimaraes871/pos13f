#' Version, creation time and inputs of every pin the report reads
#'
#' Every pin records the versions of the pins it was built from, so a reported number
#' can be traced to the exact data that produced it.
#'
#' @param board A `pins_board`.
#'
#' @return A tibble with `pin`, `version`, `created` and `n_inputs`, one row per pin on
#'   the board.
#' @noRd
report_pin_lineage <- function(board) {
  purrr::map(pins::pin_list(board), function(pin) {
    m <- pins::pin_meta(board, pin)
    tibble::tibble(
      pin = pin, version = m$local$version, created = as.character(m$created),
      n_inputs = length(m$user$inputs)
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$pin)
}
