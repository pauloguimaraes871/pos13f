# A cache holding the mini archive under its SEC name, so no request is ever made
setup_bronze <- function(env = parent.frame(), mutate_submission = NULL) {
  src <- withr::local_tempdir(.local_envir = env)
  file.copy(list.files(test_path("fixtures", "sec_mini"), full.names = TRUE), src)
  if (!is.null(mutate_submission)) {
    lines <- readLines(file.path(src, "SUBMISSION.tsv"))
    writeLines(mutate_submission(lines), file.path(src, "SUBMISSION.tsv"))
  }
  cache_dir <- withr::local_tempdir(.local_envir = env)
  zip_path <- file.path(cache_dir, "2014q1_form13f.zip")
  zip::zip(zip_path, files = list.files(src, full.names = TRUE), mode = "cherry-pick")
  list(
    board = init_pipeline_board(withr::local_tempdir(.local_envir = env)),
    cache_dir = cache_dir,
    log_path = withr::local_tempdir(.local_envir = env),
    archives = tibble::tibble(
      file = "2014q1_form13f.zip", url = "https://invalid.invalid/2014q1_form13f.zip",
      label = "2014q1", window_start = as.Date("2014-01-01"),
      window_end = as.Date("2014-03-31"), size_bytes = file.size(zip_path),
      last_modified = "Mon, 14 Apr 2014 16:43:16 GMT"
    )
  )
}

## Serve the cached zip as SEC's reply, for tests that force a re-download
mock_sec_download <- function(s, env = parent.frame()) {
  zip_path <- file.path(s$cache_dir, "2014q1_form13f.zip")
  body <- readBin(zip_path, "raw", file.size(zip_path))
  httr2::local_mocked_responses(
    function(req) httr2::response(status_code = 200, body = body),
    env = env
  )
}

## The mini archive has 3 filings and 7 holdings, so the size floors are lowered
run_deploy <- function(s) {
  suppressMessages(deploy_bronze(s$board, s$archives,
    cache_dir = s$cache_dir,
    user_agent = "Test test@example.com",
    log_path = s$log_path, min_filings = 1, min_holdings = 1, verbose = FALSE
  ))
}

test_that("deploy_bronze applies the low floor only to archives SEC publishes as partial", {
  s <- setup_bronze()
  deploy <- function(partial) {
    suppressMessages(deploy_bronze(s$board, s$archives,
      cache_dir = s$cache_dir, user_agent = "Test test@example.com",
      log_path = s$log_path, partial_archives = partial,
      partial_min_filings = 1, partial_min_holdings = 1, verbose = FALSE
    ))
  }

  ## Not declared partial: the default floors reject a 3-filing archive
  expect_error(deploy(partial = character(0)), "Tests failed")
  ## Declared partial: the low floors apply and the archive is pinned
  expect_equal(deploy(partial = "2014q1")$status, "pinned")
})

test_that("deploy_bronze pins each archive as six parquet tables with its source", {
  s <- setup_bronze()

  result <- run_deploy(s)

  expect_equal(result$status, "pinned")
  paths <- pins::pin_download(s$board, "bronze_sec_2014q1")
  expect_setequal(basename(paths), paste0(c(names(sec_table_schema()), "SOURCE"), ".parquet"))
  infotable <- nanoparquet::read_parquet(paths[basename(paths) == "INFOTABLE.parquet"])
  expect_equal(nrow(infotable), 7L)
  meta <- pins::pin_meta(s$board, "bronze_sec_2014q1")$user
  expect_equal(as.numeric(meta$source_bytes), s$archives$size_bytes)
})

test_that("deploy_bronze skips an archive already pinned from the same source file", {
  s <- setup_bronze()
  run_deploy(s)

  again <- run_deploy(s)

  expect_equal(again$status, "skipped")
  expect_equal(nrow(pins::pin_versions(s$board, "bronze_sec_2014q1")), 1L)
})

test_that("deploy_bronze re-pins an archive SEC republished with identical tables", {
  s <- setup_bronze()
  run_deploy(s)
  ## Same tables, different compression: a new source file of a different size
  zip_path <- file.path(s$cache_dir, "2014q1_form13f.zip")
  files <- list.files(test_path("fixtures", "sec_mini"), full.names = TRUE)
  zip::zip(zip_path, files = files, mode = "cherry-pick", compression_level = 1)
  s$archives$size_bytes <- file.size(zip_path)
  mock_sec_download(s)
  Sys.sleep(1.1)

  again <- run_deploy(s)

  expect_equal(again$status, "pinned")
  expect_equal(nrow(pins::pin_versions(s$board, "bronze_sec_2014q1")), 2L)
  meta <- pins::pin_meta(s$board, "bronze_sec_2014q1")$user
  expect_equal(as.numeric(meta$source_bytes), s$archives$size_bytes)
})

test_that("deploy_bronze re-pins an archive SEC revised without changing its size (Codex B02)", {
  s <- setup_bronze()
  run_deploy(s)
  s$archives$last_modified <- "Tue, 15 Apr 2014 09:00:00 GMT"
  mock_sec_download(s)
  Sys.sleep(1.1)

  again <- run_deploy(s)

  expect_equal(again$status, "pinned")
  meta <- pins::pin_meta(s$board, "bronze_sec_2014q1")$user
  expect_equal(meta$source_last_modified, "Tue, 15 Apr 2014 09:00:00 GMT")
})

test_that("deploy_bronze re-pins an archive pinned in an older format", {
  ## A pin made before SOURCE.parquet existed has six files: it must not be skipped
  s <- setup_bronze()
  dir <- withr::local_tempdir()
  old_files <- file.path(dir, paste0(names(sec_table_schema()), ".parquet"))
  purrr::walk(old_files, function(f) nanoparquet::write_parquet(data.frame(x = 1), f))
  save_local_pin(s$board, old_files, "bronze", "sec_2014q1",
    extra = list(source_bytes = s$archives$size_bytes), verbose = FALSE
  )
  mock_sec_download(s)
  Sys.sleep(1.1)

  result <- run_deploy(s)

  expect_equal(result$status, "pinned")
  expect_equal(nrow(pins::pin_versions(s$board, "bronze_sec_2014q1")), 2L)
})

test_that("deploy_bronze stops and pins nothing when the data-quality suite fails", {
  ## Duplicate the first filing: a primary-key violation
  s <- setup_bronze(mutate_submission = function(lines) c(lines, lines[2]))

  expect_error(run_deploy(s), "Tests failed")
  expect_false("bronze_sec_2014q1" %in% pins::pin_list(s$board))
  expect_length(list.files(s$log_path, pattern = "test_bronze_sec_2014q1"), 1)
})

test_that("deploy_bronze rejects an archive list without the required columns", {
  s <- setup_bronze()
  expect_error(deploy_bronze(s$board, dplyr::select(s$archives, -"size_bytes")), "archives")
})
