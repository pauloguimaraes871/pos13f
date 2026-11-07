# Offline: the SEC page and the HEAD replies are mocked
mock_sec <- function(req) {
  if (grepl("form-13f-data-sets$", req$url)) {
    html <- paste(
      '<a href="/files/structureddata/data/form-13f-data-sets/2014q1_form13f.zip">',
      '<a href="/files/structureddata/data/form-13f-data-sets/2014q2_form13f.zip">'
    )
    return(httr2::response(status_code = 200, body = charToRaw(html)))
  }
  size <- if (grepl("2014q1", req$url)) "38064793" else "38411120"
  httr2::response(
    status_code = 200,
    headers = list(
      `content-length` = size,
      `last-modified` = "Mon, 14 Apr 2014 16:43:16 GMT"
    )
  )
}

test_that("list_sec_archives returns each archive with its exact size and revision date", {
  httr2::local_mocked_responses(mock_sec)

  archives <- list_sec_archives(user_agent = "Test test@example.com", verbose = FALSE)

  expect_equal(archives$label, c("2014q1", "2014q2"))
  expect_equal(archives$size_bytes, c(38064793, 38411120))
  ## Last-Modified identifies a republication that keeps the same size (Codex B02)
  expect_equal(archives$last_modified, rep("Mon, 14 Apr 2014 16:43:16 GMT", 2))
})

test_that("list_sec_archives requires a user agent", {
  expect_error(list_sec_archives(user_agent = ""), "user_agent")
})
