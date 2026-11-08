## 150 months of a five-portfolio ladder whose means step by `step` points
mono_ladder <- function(step) {
  set.seed(2)
  months <- seq(as.Date("2010-01-01"), by = "month", length.out = 150)
  tibble::tibble(dates = rep(months, each = 5), quantile = rep(1:5, 150)) |>
    dplyr::mutate(raw_return = step * .data$quantile + stats::rnorm(dplyr::n(), 0, 1))
}

test_that("a clearly rising ladder rejects and a falling one does not", {
  rising <- test_monotonicity(mono_ladder(0.6))
  falling <- test_monotonicity(mono_ladder(-0.6))
  expect_lt(rising$p_value, 0.05)
  expect_gt(falling$p_value, 0.5)
  expect_length(rising$diffs[[1]], 4)
})

test_that("one flat step is enough to fail, however steep the others", {
  ## Q3 equals Q2 plus mean-zero noise, so the Q3 minus Q2 spread has mean exactly 0;
  ## the other steps are large (the null configuration a centred bootstrap misses)
  ladder <- mono_ladder(1)
  q2 <- ladder$raw_return[ladder$quantile == 2]
  noise <- stats::rnorm(length(q2), 0, 0.5)
  ladder$raw_return[ladder$quantile == 3] <- q2 + noise - mean(noise)
  out <- test_monotonicity(ladder)
  expect_equal(out$p_value, 0.5, tolerance = 1e-6)
})

test_that("a ladder with a missing quantile month is refused", {
  expect_error(test_monotonicity(mono_ladder(0.6)[-1, ]), "every quantile")
})
