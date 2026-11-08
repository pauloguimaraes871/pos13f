## The full deploy reads every silver pin; its logic is tested function by function and
## end to end by the pipeline run, where run_test_gold() checks the real panels
test_that("deploy_gold rejects a board that is not a pins board", {
  expect_error(deploy_gold("data/prod", verbose = FALSE), "pins board")
})
