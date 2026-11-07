test_that("download_sec_archive skips the download when the cached file has the expected size", {
  dest_dir <- withr::local_tempdir()
  cached <- file.path(dest_dir, "2014q1_form13f.zip")
  writeBin(as.raw(1:100), cached)

  ## The url is unreachable on purpose: a request would fail the test
  path <- download_sec_archive("https://invalid.invalid/2014q1_form13f.zip", dest_dir,
    user_agent = "Test test@example.com",
    expected_bytes = 100, verbose = FALSE
  )

  expect_equal(normalizePath(path), normalizePath(cached))
})

test_that("download_sec_archive rejects a download whose size differs (Codex B03)", {
  ## A truncated transfer must never enter the cache as if it were complete
  dest_dir <- withr::local_tempdir()
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, body = as.raw(1:50))
  })

  expect_error(
    download_sec_archive("https://www.sec.gov/2014q1_form13f.zip", dest_dir,
      user_agent = "Test test@example.com", expected_bytes = 100, verbose = FALSE
    ),
    "50 bytes"
  )
  expect_length(list.files(dest_dir), 0)
})

test_that("download_sec_archive with force replaces a cached file", {
  ## Used when SEC republished an archive: the cached copy is stale even if its size
  ## still matches
  dest_dir <- withr::local_tempdir()
  cached <- file.path(dest_dir, "2014q1_form13f.zip")
  writeBin(as.raw(rep(1, 100)), cached)
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, body = as.raw(rep(2, 100)))
  })

  download_sec_archive("https://www.sec.gov/2014q1_form13f.zip", dest_dir,
    user_agent = "Test test@example.com", expected_bytes = 100, force = TRUE,
    verbose = FALSE
  )

  expect_equal(readBin(cached, "raw", 100), as.raw(rep(2, 100)))
})

test_that("download_sec_archive rejects a missing user agent and bad arguments", {
  dest_dir <- withr::local_tempdir()
  url <- "https://www.sec.gov/2014q1_form13f.zip"
  expect_error(download_sec_archive(url, dest_dir, user_agent = ""), "user_agent")
  expect_error(download_sec_archive("", dest_dir, user_agent = "a b@c.d"), "url")
  expect_error(
    download_sec_archive(url, dest_dir, "a b@c.d", expected_bytes = -1),
    "expected_bytes"
  )
})

test_that("download_sec_archive downloads a real archive listed by SEC (opt-in)", {
  ## Live network test, off by default so an SEC outage cannot block the pipeline's
  ## unit-test gate (Codex B05). Enable with POS13F_LIVE_TESTS=true.
  skip_if_not(Sys.getenv("POS13F_LIVE_TESTS") == "true", "live tests not enabled")
  skip_if(Sys.getenv("SEC_USER_AGENT") == "", "SEC_USER_AGENT not set")
  dest_dir <- withr::local_tempdir()

  ## The smallest archive (2013q2, about 2 MB) keeps this test short
  path <- download_sec_archive(
    "https://www.sec.gov/files/structureddata/data/form-13f-data-sets/2013q2_form13f.zip",
    dest_dir,
    user_agent = Sys.getenv("SEC_USER_AGENT"), verbose = FALSE
  )

  expect_true(file.exists(path))
  expect_gt(file.size(path), 1e6)
})
