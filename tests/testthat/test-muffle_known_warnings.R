test_that("only the known factoRverse advisories are muffled", {
  known <- "Normalization not found in signals_m_df workflow. x"
  expect_silent(muffle_known_warnings(warning(known)))
  expect_warning(muffle_known_warnings(warning("something else")), "something else")
  expect_equal(muffle_known_warnings(1 + 1), 2)
})
