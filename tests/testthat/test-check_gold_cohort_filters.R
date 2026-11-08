cohort_block <- function(cohort = gold_cohort_fixture(), filings = gold_filings(),
                         versions = gold_versions()) {
  function() {
    check_gold_cohort_filters(cohort, filings, versions, gold_relations(), gold_formations(), 10)
  }
}

test_that("the cohort screens pass on the hand-made cohort and on the real-data toy", {
  expect_true(toy_gold_block_passes(cohort_block()))
  toy <- toy_silver_gold()
  expect_true(toy_gold_block_passes(function() {
    check_gold_cohort_filters(
      toy$cohort, toy$filings, toy$versions, toy$relations, toy$formations, 10
    )
  }))
})

test_that("a cohort filer whose report became public after t fails the block", {
  late <- dplyr::mutate(gold_filings(),
    usable_date = dplyr::if_else(
      .data$cik == "0000000001", as.Date("2019-09-05"), .data$usable_date
    )
  )
  expect_false(toy_gold_block_passes(cohort_block(filings = late)))
})

test_that("a wrong few-positions or overlap label fails the block", {
  few <- dplyr::mutate(gold_cohort_fixture(),
    excluded_reason = dplyr::if_else(
      .data$cik == "0000000003", NA_character_, .data$excluded_reason
    ),
    in_cohort = is.na(.data$excluded_reason)
  )
  expect_false(toy_gold_block_passes(cohort_block(cohort = few)))
  ## Filer 6 has no inclusion link to anyone
  overlap <- dplyr::mutate(gold_cohort_fixture(),
    excluded_reason = dplyr::if_else(.data$cik == "0000000006", "overlap", .data$excluded_reason),
    in_cohort = is.na(.data$excluded_reason)
  )
  expect_false(toy_gold_block_passes(cohort_block(cohort = overlap)))
})

test_that("a filer with both reports current at t but missing from the cohort fails", {
  missing <- dplyr::filter(gold_cohort_fixture(), .data$cik != "0000000006")
  expect_false(toy_gold_block_passes(cohort_block(cohort = missing)))
})

test_that("an overlap drop whose link was superseded before t fails the block", {
  restated <- dplyr::bind_rows(
    dplyr::mutate(gold_versions(), valid_to = dplyr::if_else(
      .data$accession_number == "0000000004-19-00002", as.Date("2019-08-20"), .data$valid_to
    )),
    tibble::tibble(
      cik = "0000000004", period_of_report = as.Date("2019-06-30"),
      accession_number = "0000000004-19-00003", valid_from = as.Date("2019-08-20"),
      valid_to = as.Date(NA), is_visible = TRUE, kind = "replace", event_no = 2L,
      base_no = 2L, usable_date = as.Date("2019-08-20")
    )
  )
  ## The fixture cohort still drops filer 5 for overlap, from the superseded report
  expect_false(toy_gold_block_passes(cohort_block(versions = restated)))
})
