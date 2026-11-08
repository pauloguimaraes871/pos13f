reference_block <- function(build) {
  function() check_gold_reference(build$panels, build$toy$cohort, build$reference)
}

test_that("sampled toy rows agree with the dplyr recomputation", {
  build <- toy_gold_build()
  expect_gte(nrow(build$reference$sample), 8)
  expect_true(toy_gold_block_passes(reference_block(build)))
})

test_that("a perturbed signal, feature or return fails the block", {
  build <- toy_gold_build()
  first <- build$reference$sample$id[1]
  nudge <- function(m_df, col) {
    hit <- m_df$id == first
    dplyr::mutate(m_df, "{col}" := dplyr::if_else(hit, .data[[col]] * 1.001 + 1e-6, .data[[col]]))
  }
  for (case in list(
    c("signals_m_df", "f1_breadth_change"), c("signals_m_df", "mom_12_1"),
    c("liquidity_m_df", "median_volfin_3m"), c("fwd_1m_returns_m_df", "fwd_return_1m")
  )) {
    broken <- build
    broken$panels[[case[1]]] <- nudge(broken$panels[[case[1]]], case[2])
    expect_false(toy_gold_block_passes(reference_block(broken)), label = case[2])
  }
})
