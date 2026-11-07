test_that("pin_sec_archive pins six tables plus SOURCE with the verified source identity", {
  board <- init_pipeline_board(withr::local_tempdir())
  zip_path <- withr::local_tempfile(fileext = "_form13f.zip")
  zip::zip(zip_path,
    files = list.files(test_path("fixtures", "sec_mini"), full.names = TRUE),
    mode = "cherry-pick"
  )
  tables <- read_sec_archive(zip_path, verbose = FALSE)
  archive <- tibble::tibble(
    url = "https://www.sec.gov/2014q1_form13f.zip", label = "2014q1",
    last_modified = "Mon, 14 Apr 2014 16:43:16 GMT"
  )

  pin_sec_archive(board, tables, archive, zip_path, verbose = FALSE)

  paths <- pins::pin_download(board, "bronze_sec_2014q1")
  expect_setequal(basename(paths), paste0(c(names(sec_table_schema()), "SOURCE"), ".parquet"))
  src <- nanoparquet::read_parquet(paths[basename(paths) == "SOURCE.parquet"])
  ## Size and hash are measured on the downloaded file, not copied from SEC's listing
  expect_equal(src$source_bytes, file.size(zip_path))
  expect_equal(src$source_md5, unname(tools::md5sum(zip_path)))
  expect_equal(src$source_last_modified, archive$last_modified)
  meta <- pins::pin_meta(board, "bronze_sec_2014q1")$user
  expect_equal(meta$source_last_modified, archive$last_modified)
})
