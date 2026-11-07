# Realistic small table: two filings, unbalanced rows, ordered by accession
holdings_fixture <- function() {
  tibble::tibble(
    accession_number = c(
      "0000950123-14-000101", "0000950123-14-000101",
      "0001104659-14-000222"
    ),
    cusip = c("037833100", "594918104", "037833100"),
    shares = c(1200, 5000, 300)
  )
}

test_that("save_local_pin writes a data frame as parquet and returns the version", {
  board <- init_pipeline_board(withr::local_tempdir())

  version <- save_local_pin(board, holdings_fixture(),
    stage = "silver",
    table = "holdings", verbose = FALSE
  )

  expect_type(version, "character")
  expect_equal(pins::pin_meta(board, "silver_holdings")$type, "parquet")
  expect_equal(pins::pin_read(board, "silver_holdings"), holdings_fixture(),
    ignore_attr = TRUE
  )
})

test_that("save_local_pin records stage, table, row count and input versions", {
  board <- init_pipeline_board(withr::local_tempdir())
  inputs <- list(bronze_sec_2014q1 = "20261107T010000Z-abc12")

  save_local_pin(board, holdings_fixture(),
    stage = "silver", table = "holdings",
    inputs = inputs, verbose = FALSE
  )

  meta <- pins::pin_meta(board, "silver_holdings")$user
  expect_equal(meta$stage, "silver")
  expect_equal(meta$table, "holdings")
  expect_equal(meta$n_rows, 3L)
  expect_equal(meta$inputs, inputs)
})

test_that("save_local_pin stores extra metadata such as the source of the data", {
  board <- init_pipeline_board(withr::local_tempdir())

  save_local_pin(board, holdings_fixture(), "bronze", "sec_2014q1",
    extra = list(source_url = "https://www.sec.gov/x.zip", source_bytes = 38064793),
    verbose = FALSE
  )

  meta <- pins::pin_meta(board, "bronze_sec_2014q1")$user
  expect_equal(meta$source_url, "https://www.sec.gov/x.zip")
  expect_equal(as.numeric(meta$source_bytes), 38064793)
  expect_error(save_local_pin(board, holdings_fixture(), "bronze", "x", extra = "y"), "extra")
})

test_that("save_local_pin keeps large numeric metadata exact", {
  ## pins writes metadata as YAML with 7 significant digits: 100719015 came back as
  ## 100719020 on 2026-11-07. Numbers must survive the round trip exactly.
  board <- init_pipeline_board(withr::local_tempdir())

  save_local_pin(board, holdings_fixture(), "bronze", "sec_2026",
    extra = list(source_bytes = 100719015), verbose = FALSE
  )

  stored <- pins::pin_meta(board, "bronze_sec_2026")$user$source_bytes
  expect_equal(as.numeric(stored), 100719015)
})

test_that("identical content creates no new version; changed content adds one", {
  board <- init_pipeline_board(withr::local_tempdir())
  first <- save_local_pin(board, holdings_fixture(), "silver", "holdings", verbose = FALSE)

  ## Same content again: pins skips the write, so still one version
  same <- save_local_pin(board, holdings_fixture(), "silver", "holdings", verbose = FALSE)
  expect_equal(nrow(pins::pin_versions(board, "silver_holdings")), 1L)
  expect_equal(same, first)

  ## Changed content: a second version, and the first stays readable (time travel)
  changed <- dplyr::mutate(holdings_fixture(), shares = .data$shares * 2)
  Sys.sleep(1.1)
  save_local_pin(board, changed, "silver", "holdings", verbose = FALSE)
  expect_equal(nrow(pins::pin_versions(board, "silver_holdings")), 2L)
  expect_equal(
    pins::pin_read(board, "silver_holdings", version = first)$shares,
    holdings_fixture()$shares
  )
})

test_that("save_local_pin uploads parquet files without reading them into R", {
  board <- init_pipeline_board(withr::local_tempdir())
  file <- file.path(withr::local_tempdir(), "SUBMISSION.parquet")
  nanoparquet::write_parquet(holdings_fixture(), file)

  save_local_pin(board, file, stage = "bronze", table = "sec_2014q1", verbose = FALSE)

  paths <- pins::pin_download(board, "bronze_sec_2014q1")
  expect_equal(basename(paths), "SUBMISSION.parquet")
  expect_equal(nrow(nanoparquet::read_parquet(paths)), 3L)
})

test_that("save_local_pin rejects invalid arguments", {
  board <- init_pipeline_board(withr::local_tempdir())
  df <- holdings_fixture()
  expect_error(save_local_pin("not a board", df, "silver", "holdings"), "board")
  expect_error(save_local_pin(board, df, "platinum", "holdings"), "stage")
  expect_error(save_local_pin(board, df, "silver", "Holdings"), "table")
  expect_error(save_local_pin(board, "missing.parquet", "silver", "holdings"), "data")
  expect_error(save_local_pin(board, df, "silver", "holdings", inputs = "v1"), "inputs")
})
