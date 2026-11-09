test_that("targets are placed in layer columns and edges get both ends", {
  nodes <- tibble::tibble(
    name = c("unit_tests", "bronze_sec", "silver_filings", "gold", "backtests", "report")
  )
  edges <- tibble::tibble(
    from = c("unit_tests", "bronze_sec", "silver_filings", "gold", "backtests", "outside"),
    to = c("bronze_sec", "silver_filings", "gold", "backtests", "report", "gold")
  )
  out <- report_network_layout(nodes, edges)
  placed <- match(c("unit_tests", "bronze_sec", "report"), out$nodes$name)
  expect_equal(out$nodes$x[placed], c(1, 2, 6))
  ## The edge from a target not in the list is dropped
  expect_equal(nrow(out$edges), 5)
  expect_true(all(out$edges$xend > out$edges$x))
})
