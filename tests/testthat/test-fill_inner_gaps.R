test_that("gaps inside the life become 0 and the edges stay missing", {
  x <- c(NA, NA, 1, NA, -2, NA, NA, 3, NA)
  expect_equal(fill_inner_gaps(x), c(NA, NA, 1, 0, -2, 0, 0, 3, NA))
  expect_equal(fill_inner_gaps(c(NA, 1, NA)), c(NA, 1, NA))
  expect_equal(fill_inner_gaps(c(NA_real_, NA_real_)), c(NA_real_, NA_real_))
})
