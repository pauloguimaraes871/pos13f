test_that("the rule has the five columns factoRverse needs and flags one group", {
  signals <- add_conditional_rank(
    tibble::tibble(
      id = paste0("S", 1:6, "-2020-03-01"), tickers = paste0("S", 1:6),
      dates = as.Date("2020-03-01"),
      f1 = 1:6, mom = 6:1
    ),
    "f1", "mom"
  )
  rule <- build_group_rule(signals, "mom_group", 2)
  expect_s4_class(rule, "meta_dataframe")
  expect_equal(names(rule@data), c("id", "tickers", "dates", "group_label", "mom_group_2"))
  expect_equal(sum(rule@data$mom_group_2), 2)
  expect_true(is.character(rule@data$group_label))
})
