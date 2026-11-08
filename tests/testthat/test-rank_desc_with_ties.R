test_that("ranks go from largest to smallest, ties by id, unranked rows NA", {
  ranks <- rank_desc_with_ties(c(5, 9, 5, 1), c("b", "c", "a", "d"), c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(ranks, c(3L, 1L, 2L, NA))
})

test_that("rank_desc_with_ties rejects mismatched lengths and NA values to rank", {
  expect_error(rank_desc_with_ties(1:2, "a", c(TRUE, TRUE)), "same length")
  expect_error(rank_desc_with_ties(c(1, NA), c("a", "b"), c(TRUE, TRUE)), "NA")
})
