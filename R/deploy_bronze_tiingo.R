#' Deploy the bronze Tiingo layer: daily prices of every mapped US common stock
#'
#' Requests the full daily history of each ticker from `list_tiingo_tickers()` (US
#' common stocks from `bronze_openfigi`, plus SPY), caching each ticker on arrival, then
#' runs [run_test_bronze_tiingo()] and pins all prices as one parquet file under
#' `bronze_tiingo_prices`, with the `bronze_openfigi` version as input.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param window_end Date scalar, last day of the SEC filing window of this run, set by
#'   the user in `_targets.R`; recorded in the pin metadata.
#' @param force Logical scalar; `TRUE` deletes the cache and redownloads every ticker.
#'   Required when `window_end` is later than the one recorded in the current pin.
#' @param cache_dir Character scalar, folder with one parquet file per ticker.
#' @param start_date Character scalar, first date requested (`YYYY-MM-DD`).
#' @param api_key Character scalar, Tiingo API key.
#' @param log_path Character scalar, folder for the data-quality log.
#' @param min_found_share,min_spy_rows,max_bad_adj_share Numeric scalars passed to
#'   [run_test_bronze_tiingo()].
#' @param verbose Logical; print progress.
#'
#' @return Character scalar, the version of `bronze_tiingo_prices`.
#'
#' @details
#' Resumable: a ticker already cached is not requested again, so an interrupted run
#' continues where it stopped. Cache files are named `t_<ticker>.parquet` because
#' Windows forbids file names such as `CON` or `PRN`, which are real tickers. duckdb
#' combines the files and computes the checks, so the full table never enters R.
#'
#' The cache is never extended: prices of live tickers end at their download date, and
#' Tiingo restates adjusted prices after splits and dividends, so new dates cannot be
#' appended to old ones. A new SEC batch (a later `window_end`) therefore stops the
#' deploy until it is rerun with `force = TRUE`, which redownloads everything
#' (`check_tiingo_window()`).
#' @export
deploy_bronze_tiingo <- function(board, window_end, force = FALSE,
                                 cache_dir = file.path("data-raw", "cache", "tiingo"),
                                 start_date = "2013-01-01",
                                 api_key = Sys.getenv("TIINGO_API_KEY"),
                                 log_path = file.path("logs", "tests"),
                                 min_found_share = 0.4, min_spy_rows = 3000,
                                 max_bad_adj_share = 0.001, verbose = TRUE) {
  # Validate inputs
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")

  # Reuse or refresh the cache, given the window it was pinned for
  previous_end <- if ("bronze_tiingo_prices" %in% pins::pin_list(board)) {
    pins::pin_meta(board, "bronze_tiingo_prices")$user$window_end
  }
  if (!is.null(previous_end)) previous_end <- as.Date(previous_end)
  if (check_tiingo_window(window_end, previous_end, force) == "refresh") {
    unlink(list.files(cache_dir, pattern = "^t_.*\\.parquet$", full.names = TRUE))
  }

  # Tickers to request, and those not yet cached
  replies <- pins::pin_read(board, "bronze_openfigi")
  inputs <- list(bronze_openfigi = pins::pin_meta(board, "bronze_openfigi")$local$version)
  tickers <- list_tiingo_tickers(replies)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- function(t) file.path(cache_dir, paste0("t_", t, ".parquet"))
  todo <- tickers$ticker[!file.exists(cache_file(tickers$ticker))]

  # Download missing tickers, caching each on arrival
  ## A for loop writes each ticker to disk before the next request, so an interrupted
  ## night loses at most one ticker
  for (i in seq_along(todo)) {
    px <- download_tiingo_prices(todo[i], start_date = start_date, api_key = api_key)
    nanoparquet::write_parquet(px, cache_file(todo[i]))
    if (verbose && i %% 500 == 0) message("Tiingo: ", i, " of ", length(todo), " tickers")
  }

  # Combine and summarise inside duckdb
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  files_sql <- paste0("'", normalizePath(cache_file(tickers$ticker), winslash = "/"), "'",
    collapse = ", "
  )
  ## union_by_name: 404 files hold only ticker, status and date
  prices <- dplyr::tbl(con, dplyr::sql(paste0(
    "SELECT * FROM read_parquet([", files_sql, "], union_by_name = true)"
  )))
  summary <- prices |>
    ## Bad prices on priced rows, counted apart: a raw close must be positive, while an
    ## adjusted close of 0 is a known vendor defect (cumulative adjustment of tiny OTC
    ## prices rounds to 0, 9,680 rows of 13 tickers on 2026-11-07) that silver drops
    dplyr::mutate(
      priced = .data$status == "200",
      bad_close = .data$priced & (is.na(.data$close) | .data$close <= 0),
      bad_adj = .data$priced & (is.na(.data$adj_close) | .data$adj_close <= 0)
    ) |>
    dplyr::summarise(
      n_rows = dplyr::n(), n_dates = dplyr::n_distinct(.data$date),
      n_bad_close = sum(dplyr::if_else(.data$bad_close, 1, 0)),
      n_bad_adj = sum(dplyr::if_else(.data$bad_adj, 1, 0)),
      .by = c("ticker", "status")
    ) |>
    dplyr::collect()

  # Test, then write one parquet file and pin it
  ok <- run_logged_test(
    function() {
      run_test_bronze_tiingo(summary, tickers,
        min_found_share = min_found_share, min_spy_rows = min_spy_rows,
        max_bad_adj_share = max_bad_adj_share
      )
    },
    log_file = "test_bronze_tiingo", log_path = log_path
  )
  check_test_success(ok)
  out <- file.path(tempfile("tiingo_"), "TIINGO_PRICES.parquet")
  dir.create(dirname(out))
  on.exit(unlink(dirname(out), recursive = TRUE), add = TRUE)
  DBI::dbExecute(con, paste0(
    "COPY (SELECT * FROM read_parquet([", files_sql, "], union_by_name = true) ",
    "ORDER BY ticker, date) TO '", normalizePath(out, winslash = "/", mustWork = FALSE),
    "' (FORMAT PARQUET)"
  ))
  ## The defect count travels with the pin so the memo can disclose it
  save_local_pin(board, out, "bronze", "tiingo_prices",
    inputs = inputs,
    extra = list(
      window_end = as.character(window_end), n_bad_adj_rows = sum(summary$n_bad_adj)
    ),
    verbose = verbose
  )
}
