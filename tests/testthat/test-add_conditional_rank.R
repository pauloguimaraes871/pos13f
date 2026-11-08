## Two months of nine stocks; the conditioner splits them in thirds
rank_fixture <- function() {
  tibble::tibble(
    id = paste0("S", rep(1:9, 2), "-", rep(c("a", "b"), each = 9)),
    tickers = paste0("S", rep(1:9, 2)),
    dates = rep(as.Date(c("2020-03-01", "2020-06-01")), each = 9),
    f1 = c(9:1, 1:9) / 10, mom = rep(1:9, 2)
  )
}

test_that("ranks run within each conditioner group and date", {
  out <- add_conditional_rank(rank_fixture(), "f1", "mom")
  expect_equal(sort(unique(out$mom_group)), c(1, 2, 3))
  ## Within every group of three, ranks are 1/3, 2/3 and 1
  per_group <- dplyr::summarise(out, r = list(sort(.data$f1_in_mom)), .by = c("dates", "mom_group"))
  expect_true(all(purrr::map_lgl(per_group$r, \(r) isTRUE(all.equal(r, c(1, 2, 3) / 3)))))
  ## In March F1 falls with momentum, so the top-momentum group's highest F1 is S7
  march_top <- dplyr::filter(out, .data$dates == as.Date("2020-03-01"), .data$mom_group == 3)
  expect_equal(march_top$tickers[march_top$f1_in_mom == 1], "S7")
})

test_that("missing columns or missing values are refused", {
  expect_error(add_conditional_rank(rank_fixture(), "f1", "nope"), "missing columns")
  bad <- dplyr::mutate(rank_fixture(),
    mom = dplyr::if_else(dplyr::row_number() == 1, NA, .data$mom)
  )
  expect_error(add_conditional_rank(bad, "f1", "mom"), "must not be NA")
})
