## Membership bounds are loose here: eight members turn over fast in a three-formation toy
universe_block <- function(panels) {
  function() check_gold_universe_filters(panels, 8, 5, 300, 1, 1)
}

test_that("the universe screens pass on the real-data toy", {
  build <- toy_gold_build()
  expect_true(toy_gold_block_passes(universe_block(build$panels)))
})

test_that("a member below the price floor, or a wrong reason, fails the block", {
  build <- toy_gold_build()
  cheap <- build$panels
  first <- which(cheap$universe_diagnostics$in_universe)[1]
  cheap$universe_diagnostics <- dplyr::mutate(cheap$universe_diagnostics,
    close_prev = dplyr::if_else(dplyr::row_number() == first, 4, .data$close_prev)
  )
  expect_false(toy_gold_block_passes(universe_block(cheap)))
  relabelled <- build$panels
  relabelled$universe_diagnostics <- dplyr::mutate(relabelled$universe_diagnostics,
    excluded_reason = dplyr::if_else(
      .data$excluded_reason %in% "price_below_min", "short_history", .data$excluded_reason
    )
  )
  expect_false(toy_gold_block_passes(universe_block(relabelled)))
})

test_that("a panel row for a non-member fails the block", {
  build <- toy_gold_build()
  panels <- build$panels
  outsider <- dplyr::filter(panels$universe_diagnostics, !.data$in_universe)$instrument_id[1]
  extra <- dplyr::mutate(panels$signals_m_df[1, ],
    tickers = outsider, id = paste0(outsider, "-", .data$dates)
  )
  panels$signals_m_df <- dplyr::bind_rows(panels$signals_m_df, extra)
  expect_false(toy_gold_block_passes(universe_block(panels)))
})
