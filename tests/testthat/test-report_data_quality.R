test_that("recorded counts are collected, and a missing field is NA", {
  meta <- function(pin) list(n_rows = "10", n_bad_adj_rows = 2)
  out <- report_data_quality(meta)
  expect_equal(out$value[out$pin == "bronze_tiingo_prices"], c(10, 2))
  expect_true(all(is.na(out$value[out$field == "n_identity_suspects"])))
})
