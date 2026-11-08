#' Lay out the targets pipeline as layered columns for a static diagram
#'
#' `targets::tar_visnetwork()` draws an interactive widget that does not survive every
#' report format; this places each target in a column by layer (setup, bronze, silver,
#' gold, backtests, reports) and spreads targets vertically within a column, so the
#' graph can be drawn with plain line and label layers.
#'
#' @param nodes Tibble with `name` (target names), as in `targets::tar_network()$vertices`.
#' @param edges Tibble with `from` and `to`, as in `targets::tar_network()$edges`.
#'
#' @return A list: `nodes` (tibble with `name`, `layer`, `x` and `y`) and `edges` (tibble
#'   with `from`, `to`, `x`, `y`, `xend`, `yend`), both restricted to the targets listed.
#' @noRd
report_network_layout <- function(nodes, edges) {
  layers <- c("setup", "bronze", "silver", "gold", "backtests", "reports")
  layer_of <- function(name) {
    dplyr::case_when(
      name %in% c("unit_tests", "board_path", "sec_archives", "sec_archives_window") ~ "setup",
      startsWith(name, "bronze") ~ "bronze",
      startsWith(name, "silver") | name == "filing_date_audit" ~ "silver",
      name == "gold" ~ "gold",
      name == "backtests" ~ "backtests",
      .default = "reports"
    )
  }
  placed <- tibble::tibble(name = nodes$name) |>
    dplyr::mutate(layer = layer_of(.data$name), x = match(.data$layer, layers)) |>
    dplyr::arrange(.data$x, .data$name) |>
    dplyr::mutate(y = dplyr::row_number() - (dplyr::n() + 1) / 2, .by = "layer")
  coords <- dplyr::select(placed, "name", "x", "y")
  lines <- edges |>
    dplyr::filter(.data$from %in% placed$name, .data$to %in% placed$name) |>
    dplyr::inner_join(dplyr::rename(coords, from = "name"),
      by = dplyr::join_by("from"), relationship = "many-to-one"
    ) |>
    dplyr::inner_join(dplyr::rename(coords, to = "name", xend = "x", yend = "y"),
      by = dplyr::join_by("to"), relationship = "many-to-one"
    )
  list(nodes = placed, edges = lines)
}
