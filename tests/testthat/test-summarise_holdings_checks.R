test_that("summarise_holdings_checks counts problems and conserves shares", {
  checks <- checks_fixture()
  totals <- checks$totals

  expect_equal(totals$n_rows, nrow(holdings_state_fixture()))
  expect_equal(totals$n_duplicate_keys, 0)
  expect_equal(totals$n_negative, 0)
  expect_equal(totals$shares_single_source_state, totals$shares_single_source_positions)
  ## Every bronze row counts, options and bonds included
  first <- dplyr::filter(checks$entry_counts, .data$accession_number == "0000000101-22-000001")
  expect_equal(first$n_infotable_rows, 5L)
})

test_that("summarise_holdings_checks sees a duplicated row and a lost one", {
  state <- holdings_state_fixture()
  doubled <- checks_fixture(dplyr::bind_rows(state, state[1, ]))$totals
  expect_equal(doubled$n_duplicate_keys, 1)
  expect_gt(doubled$shares_single_source_state, doubled$shares_single_source_positions)
  lost <- checks_fixture(state[-1, ])$totals
  expect_lt(lost$shares_single_source_state, lost$shares_single_source_positions)
})
