aug <- as.Date("2026-08-31")
nov <- as.Date("2026-11-30")

test_that("an empty cache takes the window of this run", {
  ## Every ticker is downloaded now, whatever an older record says
  expect_equal(check_tiingo_window(nov, n_cached = 0, cache_end = aug), nov)
  expect_equal(check_tiingo_window(aug, n_cached = 0), aug)
})

test_that("the same or a shorter window reuses the cache and keeps its window", {
  expect_equal(check_tiingo_window(aug, n_cached = 10, cache_end = aug), aug)
  expect_equal(check_tiingo_window(as.Date("2026-05-31"), 10, cache_end = aug), aug)
})

test_that("a later window stops and asks to delete the cache", {
  ## The next SEC batch (Sep to Nov 2026) would mix two vintages of adjusted prices
  expect_error(check_tiingo_window(nov, n_cached = 10, cache_end = aug), "Delete")
})

test_that("the cache record takes precedence over the manifest", {
  ## Cache deleted and partly redownloaded for November; the manifest still says August
  expect_equal(check_tiingo_window(nov, 10, cache_end = nov, manifest_end = aug), nov)
  ## A cache filled before it recorded its window falls back to the manifest
  expect_equal(check_tiingo_window(aug, 10, manifest_end = aug), aug)
  expect_error(check_tiingo_window(nov, 10, manifest_end = aug), "later")
})

test_that("a filled cache without any record stops", {
  expect_error(check_tiingo_window(aug, n_cached = 10), "unknown window")
})

test_that("check_tiingo_window rejects invalid arguments", {
  expect_error(check_tiingo_window("2026-08-31", 0), "window_end")
  expect_error(check_tiingo_window(as.Date(NA), 0), "window_end")
  expect_error(check_tiingo_window(aug, -1), "n_cached")
  expect_error(check_tiingo_window(aug, 1, cache_end = "2026-08-31"), "cache_end")
})
