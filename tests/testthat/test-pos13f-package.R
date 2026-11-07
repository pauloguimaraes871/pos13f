test_that("package imports the rlang pronouns used in data-masking verbs", {
  rlang_imports <- base::getNamespaceImports("pos13f")$rlang
  expect_true(all(c(".data", ".env") %in% rlang_imports))
})
