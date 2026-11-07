#' Count the lines of a text file without loading it into memory
#'
#' Used by the bronze checks to prove that every data line of a source file became a
#' row (lines minus the header must equal rows read). The file is scanned in chunks of
#' bytes, so a 360 MB INFOTABLE file never sits in memory as text.
#'
#' @param path Character scalar, path to an existing text file.
#' @param chunk_bytes Numeric scalar, bytes read per chunk.
#'
#' @return Numeric scalar: number of newline characters, plus one if the last line has
#'   no trailing newline. 0 for an empty file.
#' @noRd
count_file_lines <- function(path, chunk_bytes = 64 * 1024^2) {
  # Validate inputs
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("path must be the path of an existing file.")
  }

  # Count newline bytes chunk by chunk
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  n_newlines <- 0
  last_byte <- as.raw(10)
  repeat {
    chunk <- readBin(con, what = "raw", n = chunk_bytes)
    if (length(chunk) == 0) break
    n_newlines <- n_newlines + sum(chunk == as.raw(10))
    last_byte <- chunk[length(chunk)]
  }

  ## A final line without "\n" is still a line
  if (file.size(path) > 0 && last_byte != as.raw(10)) n_newlines + 1 else n_newlines
}
