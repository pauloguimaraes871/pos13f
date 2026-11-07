#' List the distinct share CUSIPs held across all bronze SEC archives
#'
#' The request set for OpenFIGI. duckdb reads the INFOTABLE parquet files of the
#' current version of every `bronze_sec_*` pin directly (about 150 million rows never
#' enter R); only the distinct CUSIPs are collected.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#'
#' @return A tibble ordered by `cusip` with `cusip` (character, 9 upper-case
#'   characters) and `id_type` (`"ID_CUSIP"`, or `"ID_CINS"` when the first character
#'   is a letter: issuer incorporated outside the US, which OpenFIGI only maps under
#'   that type, G1 spike). Attribute `inputs`: named list of the bronze pin versions
#'   read (lineage). Attribute `n_malformed`: number of 9-character codes dropped for a
#'   wrong check digit.
#'
#' @details
#' Only share positions are kept (`SSHPRNAMTTYPE == "SH"`, no `PUTCALL`): bonds and
#' options are never part of the signal. CUSIPs are trimmed and upper-cased because
#' filers type some in lower case (G1 spike); identifiers that are not 9
#' alphanumeric characters, or whose check digit is wrong (`is_valid_cusip()`: about
#' 20% of distinct strings, 0.09% of share positions), cannot be mapped and are dropped.
#' @noRd
list_bronze_cusips <- function(board) {
  # Find the bronze SEC pins and the INFOTABLE file of each current version
  pins_sec <- grep("^bronze_sec_", pins::pin_list(board), value = TRUE)
  if (length(pins_sec) == 0) stop("The board has no bronze_sec_* pin: run deploy_bronze() first.")
  paths <- purrr::map_chr(pins_sec, function(p) {
    files <- pins::pin_download(board, p)
    files[basename(files) == "INFOTABLE.parquet"]
  })
  inputs <- stats::setNames(
    purrr::map(pins_sec, function(p) pins::pin_meta(board, p)$local$version),
    pins_sec
  )

  # Distinct share CUSIPs, computed inside duckdb
  ## In-memory engine; shared_home = FALSE keeps duckdb from writing to ~/.duckdb
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  files_sql <- paste0("'", normalizePath(paths, winslash = "/"), "'", collapse = ", ")
  infotable <- dplyr::tbl(con, dplyr::sql(paste0("SELECT * FROM read_parquet([", files_sql, "])")))
  cusips <- infotable |>
    dplyr::filter(.data$SSHPRNAMTTYPE == "SH", is.na(.data$PUTCALL)) |>
    dplyr::transmute(cusip = toupper(trimws(.data$CUSIP))) |>
    dplyr::distinct() |>
    dplyr::collect()

  # Keep well-formed identifiers and set the OpenFIGI identifier type
  ## A wrong check digit means the filer typed the code wrongly; OpenFIGI rejects it as
  ## "Invalid idValue format" and no source could map it
  candidates <- dplyr::filter(cusips, stringr::str_detect(.data$cusip, "^[0-9A-Z]{9}$"))
  valid <- dplyr::filter(candidates, is_valid_cusip(.data$cusip))
  valid <- valid |>
    dplyr::mutate(id_type = dplyr::if_else(
      stringr::str_detect(.data$cusip, "^[A-Z]"), "ID_CINS", "ID_CUSIP"
    )) |>
    dplyr::arrange(.data$cusip)
  structure(valid, inputs = inputs, n_malformed = nrow(candidates) - nrow(valid))
}
