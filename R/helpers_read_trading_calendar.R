#' Read the trading calendar: the dates SPY traded, from the bronze Tiingo prices
#'
#' SPY trades on every NYSE session, so its price dates are the US equity trading
#' calendar for the whole window, without a separate holiday table.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#'
#' @return A tibble with one column `date` (Date, strictly increasing). Attribute
#'   `inputs`: list with the `bronze_tiingo_prices` version read (lineage).
#' @noRd
read_trading_calendar <- function(board) {
  # Validate inputs
  if (!"bronze_tiingo_prices" %in% pins::pin_list(board)) {
    stop("The board has no bronze_tiingo_prices pin: run deploy_bronze_tiingo() first.")
  }

  # SPY dates, read inside duckdb (the price file holds about 28 million rows)
  path <- pins::pin_download(board, "bronze_tiingo_prices")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  prices <- dplyr::tbl(con, dplyr::sql(paste0(
    "SELECT * FROM read_parquet('", normalizePath(path, winslash = "/"), "')"
  )))
  calendar <- prices |>
    dplyr::filter(.data$ticker == "SPY", .data$status == "200", !is.na(.data$date)) |>
    dplyr::distinct(.data$date) |>
    dplyr::arrange(.data$date) |>
    dplyr::collect() |>
    dplyr::mutate(date = as.Date(.data$date))
  inputs <- list(bronze_tiingo_prices = pins::pin_meta(board, "bronze_tiingo_prices")$local$version)
  structure(calendar, inputs = inputs)
}
