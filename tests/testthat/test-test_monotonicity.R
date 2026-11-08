## 150 months of a five-portfolio ladder whose means step by `step` points
mono_ladder <- function(step) {
  set.seed(2)
  months <- seq(as.Date("2010-01-01"), by = "month", length.out = 150)
  tibble::tibble(dates = rep(months, each = 5), quantile = rep(1:5, 150)) |>
    dplyr::mutate(raw_return = step * .data$quantile + stats::rnorm(dplyr::n(), 0, 1))
}

test_that("a clearly rising ladder rejects and a falling one does not", {
  rising <- test_monotonicity(mono_ladder(0.6), n_boot = 300)
  falling <- test_monotonicity(mono_ladder(-0.6), n_boot = 300)
  expect_lt(rising$p_value, 0.05)
  expect_gt(falling$p_value, 0.5)
  expect_length(rising$diffs[[1]], 4)
})
