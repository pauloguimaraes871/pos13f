candidates_fixture <- function(n_four = 12, n_five = 10) {
  tibble::tibble(
    formation_date = as.Date("2019-08-30"), cik = sprintf("%010d", c(4, 5, 6)),
    n_prev = c(12, 10, 11), n_curr = c(n_four, n_five, 11)
  )
}
drops_of <- function(candidates = candidates_fixture(), relations = gold_relations(),
                     versions = gold_versions()) {
  find_overlap_drops(candidates, gold_formations(), relations, versions)
}

test_that("the smaller report of an overlapping pair is dropped", {
  expect_equal(drops_of()$cik, "0000000005")
})

test_that("a tie drops the included manager", {
  expect_equal(drops_of(candidates_fixture(n_five = 12))$cik, "0000000005")
})

test_that("a relation in a report filed after t is ignored", {
  versions <- dplyr::mutate(gold_versions(), valid_from = dplyr::if_else(
    .data$accession_number == "0000000004-19-00002", as.Date("2019-09-03"), .data$valid_from
  ))
  expect_equal(nrow(drops_of(versions = versions)), 0)
})

test_that("an inclusion removed by a restatement before t no longer drops anyone", {
  ## Filer 4 restates its q report on 2019-08-20, without filer 5 among its managers
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
  expect_equal(nrow(drops_of(versions = restated)), 0)
})

test_that("a NEW HOLDINGS amendment keeps the inclusion of its base report", {
  appended <- dplyr::bind_rows(
    dplyr::mutate(gold_versions(), valid_to = dplyr::if_else(
      .data$accession_number == "0000000004-19-00002", as.Date("2019-08-20"), .data$valid_to
    )),
    tibble::tibble(
      cik = "0000000004", period_of_report = as.Date("2019-06-30"),
      accession_number = "0000000004-19-00003", valid_from = as.Date("2019-08-20"),
      valid_to = as.Date(NA), is_visible = TRUE, kind = "append", event_no = 2L,
      base_no = 1L, usable_date = as.Date("2019-08-20")
    )
  )
  expect_equal(drops_of(versions = appended)$cik, "0000000005")
})

test_that("an overlap in q-1 drops the filer, sized by its q-1 report", {
  ## Filer 6 includes filer 4 in q-1 only; filer 6 had 11 positions then, filer 4 had 12
  relations <- dplyr::bind_rows(
    gold_relations(),
    dplyr::mutate(gold_relations(),
      accession_number = "0000000006-19-00001", related_cik = "0000000004"
    )
  )
  expect_setequal(drops_of(relations = relations)$cik, c("0000000005", "0000000006"))
})
