## A board with the four inputs of the prices layer, built from the fixtures
setup_prices_board <- function(env = parent.frame()) {
  board <- init_pipeline_board(withr::local_tempdir(.local_envir = env))
  dir <- withr::local_tempdir(.local_envir = env)
  pin_file <- function(df, table) {
    path <- file.path(dir, paste0(table, ".parquet"))
    nanoparquet::write_parquet(df, path)
    path
  }
  save_local_pin(board, replies_fixture(), "bronze", "openfigi", verbose = FALSE)
  save_local_pin(board, pin_file(bronze_prices_fixture(), "TIINGO_PRICES"), "bronze",
    "tiingo_prices",
    verbose = FALSE
  )
  save_local_pin(board, dplyr::select(prices_calendar_fixture(), "date"), "silver",
    "trading_calendar",
    verbose = FALSE
  )
  state <- tibble::tibble(
    cik = "1", period_of_report = as.Date("2020-06-30"), cusip = "000000AA1",
    valid_from = as.Date("2020-08-03"), valid_to = as.Date(NA), shares = 100,
    value_usd = 1000, n_sources = 1L
  )
  save_local_pin(board, pin_file(state, "HOLDINGS_STATE"), "silver", "holdings_state",
    verbose = FALSE
  )
  board
}

test_that("deploy_silver_prices builds every table and pins nothing when a check fails", {
  ## The fixture has no AAPL, so the split specimen cannot pass
  board <- setup_prices_board()
  log_path <- withr::local_tempdir()

  expect_error(
    suppressMessages(deploy_silver_prices(board,
      log_path = log_path, temp_dir = withr::local_tempdir(), verbose = FALSE
    )),
    "Tests failed"
  )
  expect_false(any(grepl("^silver_prices_d$|^silver_security_master$", pins::pin_list(board))))
  ## Only the return evidence the fixture lacks (no AAPL, no dividend or split day)
  ## failed; every structural check passed
  log <- readLines(list.files(log_path, full.names = TRUE))
  failures <- grep("Failure", log, value = TRUE)
  expect_gte(length(failures), 1)
  expect_true(all(grepl("returns are adjusted", failures)))
})

test_that("deploy_silver_prices rejects a board that is not a pins board", {
  expect_error(deploy_silver_prices("data/prod", verbose = FALSE), "pins board")
})
