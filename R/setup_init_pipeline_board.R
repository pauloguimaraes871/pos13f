#' Initialise the versioned pins board that stores every pipeline layer
#'
#' Every layer (bronze, silver, gold, backtests, reports) is stored as pins on one
#' folder board. The board is versioned: a pin whose content changes gets a new version
#' folder and older versions stay readable, which gives time travel over every layer.
#'
#' @param path Character scalar. Folder of the board, relative to the project root or
#'   absolute. Created if missing; an existing board is reused as is.
#'
#' @return A `pins_board_folder` object with `versioned = TRUE`.
#' @export
init_pipeline_board <- function(path = file.path("data", "prod")) {
  # Validate inputs
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("path must be a single non-empty character string (the board folder).")
  }

  # Create or reuse the board
  ## dir.create is a no-op on an existing folder, so pins already there are kept
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  pins::board_folder(path, versioned = TRUE)
}
