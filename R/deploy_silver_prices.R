#' Deploy the silver identity and price layer
#'
#' Builds the security master (CUSIP to composite FIGI), then inside duckdb the daily
#' prices, returns, point-in-time illiquidity flags, terminal events and the
#' implied-price identity check; runs `run_test_silver_prices()` and pins the six tables
#' only if every check passes.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param log_path Character scalar, folder for the data-quality log.
#' @param memory_limit Character scalar, duckdb memory limit.
#' @param temp_dir Character scalar, folder where duckdb spills and writes work files.
#' @param verbose Logical; print progress and pins messages.
#'
#' @return A tibble with one row per pinned table: `table` and `version`.
#'
#' @details
#' Pins `silver_security_master`, `silver_prices_d`, `silver_returns_d`,
#' `silver_liquidity_d` (the illiquid flags), `silver_terminal_events` and
#' `silver_identity_checks`. Security-level tables carry `id`
#' (`<instrument_id>_<date>`), `instrument_id`, `date` and `ticker`, so they join on one
#' key. Metadata records the CUSIPs left out as ambiguous and the instruments whose
#' filers mostly disagree with Tiingo's price over their whole history (identity
#' suspects: a diagnostic only, since it uses later quarters; gold accepts a price
#' series quarter by quarter from `silver_identity_checks`, memo deviation D2).
#' @export
deploy_silver_prices <- function(board, log_path = file.path("logs", "tests"),
                                 memory_limit = "3GB",
                                 temp_dir = file.path("data-raw", "cache", "duckdb_tmp"),
                                 verbose = TRUE) {
  # Validate inputs
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")

  # Security master and calendar, in R
  replies <- pins::pin_read(board, "bronze_openfigi")
  master <- build_silver_security_master(replies)
  calendar <- pins::pin_read(board, "silver_trading_calendar") |>
    dplyr::mutate(day_index = dplyr::row_number())

  # Price tables, inside duckdb, each written once to a work file
  con <- open_duckdb_work(memory_limit, temp_dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  work_dir <- tempfile("prices_", tmpdir = temp_dir)
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  duckdb::duckdb_register(con, "master", master)
  duckdb::duckdb_register(con, "calendar", calendar)
  rel <- function(path) paste0("read_parquet('", path, "')")
  write <- function(sql, name) {
    path <- normalizePath(file.path(work_dir, paste0(name, ".parquet")),
      winslash = "/", mustWork = FALSE
    )
    DBI::dbExecute(con, paste0("COPY (", sql, ") TO '", path, "' (FORMAT PARQUET)"))
    path
  }
  bronze <- normalizePath(pins::pin_download(board, "bronze_tiingo_prices"), winslash = "/")
  state <- normalizePath(pins::pin_download(board, "silver_holdings_state"), winslash = "/")
  paths <- list(prices = write(sql_silver_prices_d(rel(bronze), "master", "calendar"), "PRICES_D"))
  paths$returns <- write(sql_silver_returns_d(rel(paths$prices)), "RETURNS_D")
  paths$liquidity <- write(sql_silver_liquidity_d(rel(paths$prices)), "LIQUIDITY_D")
  paths$terminal <- write(
    sql_silver_terminal_events(rel(paths$prices), max(calendar$date)), "TERMINAL"
  )
  paths$identity <- write(
    sql_silver_identity_checks(rel(state), "master", rel(paths$prices)), "IDENTITY"
  )
  if (verbose) message("Silver prices: tables written")

  # Test, then pin
  checks <- summarise_silver_prices_checks(con, c(purrr::map(paths, rel), calendar = "calendar"))
  ok <- run_logged_test(function() run_test_silver_prices(master, checks),
    log_file = "test_silver_prices", log_path = log_path
  )
  check_test_success(ok)
  identity <- tibble::as_tibble(DBI::dbGetQuery(con, paste("SELECT * FROM", rel(paths$identity))))
  suspects <- identity |>
    dplyr::summarise(match = mean(.data$n_match > .data$n_filers / 2), .by = "instrument_id") |>
    dplyr::filter(.data$match < 0.5)
  inputs <- purrr::map(
    c(
      bronze_openfigi = "bronze_openfigi", bronze_tiingo_prices = "bronze_tiingo_prices",
      silver_trading_calendar = "silver_trading_calendar",
      silver_holdings_state = "silver_holdings_state"
    ),
    function(p) pins::pin_meta(board, p)$local$version
  )
  pin <- function(data, table, extra = list()) {
    save_local_pin(board, data, "silver", table, inputs = inputs, extra = extra, verbose = verbose)
  }
  terminal <- tibble::as_tibble(DBI::dbGetQuery(con, paste("SELECT * FROM", rel(paths$terminal))))
  tibble::tibble(
    table = c(
      "security_master", "prices_d", "returns_d", "liquidity_d", "terminal_events",
      "identity_checks"
    ),
    version = c(
      pin(tibble::as_tibble(structure(master, n_ambiguous_cusips = NULL)), "security_master",
        extra = list(n_ambiguous_cusips = attr(master, "n_ambiguous_cusips"))
      ),
      pin(paths$prices, "prices_d"),
      pin(paths$returns, "returns_d"),
      pin(paths$liquidity, "liquidity_d"),
      pin(dplyr::arrange(terminal, .data$instrument_id), "terminal_events"),
      pin(dplyr::arrange(identity, .data$cusip, .data$period_of_report), "identity_checks",
        extra = list(
          n_identity_suspects = nrow(suspects),
          share_rows_thousand = sum(identity$n_thousand) / sum(identity$n_filers),
          share_rows_thousandth = sum(identity$n_thousandth) / sum(identity$n_filers)
        )
      )
    )
  )
}
