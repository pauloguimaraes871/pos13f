# Excerpt of the SEC data set page: relative links, both naming formats, a duplicate
page_fixture <- paste(
  '<a href="/files/structureddata/data/form-13f-data-sets/01mar2025-31may2025_form13f.zip">',
  '<a href="/files/structureddata/data/form-13f-data-sets/2014q1_form13f.zip">',
  '<a href="/files/structureddata/data/form-13f-data-sets/2014q1_form13f.zip">',
  '<a href="/files/other/financial_statements_2014q1.zip">'
)

test_that("parse_sec_archive_links extracts each 13F archive once with an absolute url", {
  archives <- parse_sec_archive_links(page_fixture)

  expect_equal(archives$file, c("2014q1_form13f.zip", "01mar2025-31may2025_form13f.zip"))
  expect_true(all(startsWith(archives$url, "https://www.sec.gov/files/")))
})

test_that("parse_sec_archive_links derives a label and the filing window of each archive", {
  archives <- parse_sec_archive_links(page_fixture)

  expect_equal(archives$label, c("2014q1", "01mar2025_31may2025"))
  ## Quarter archives cover the calendar quarter of filing dates
  expect_equal(archives$window_start[1], as.Date("2014-01-01"))
  expect_equal(archives$window_end[1], as.Date("2014-03-31"))
  ## Window archives state their dates in the file name
  expect_equal(archives$window_start[2], as.Date("2025-03-01"))
  expect_equal(archives$window_end[2], as.Date("2025-05-31"))
})

test_that("parse_sec_archive_links errors when the page has no 13F archive", {
  expect_error(parse_sec_archive_links("<html>maintenance</html>"), "No Form 13F")
})
