test_that("terminal events give each instrument's price span and longest gap", {
  con <- prices_con_fixture()
  last_day <- max(prices_calendar_fixture()$date)
  ## BBB stops trading a month before the end of the calendar
  DBI::dbExecute(con, paste(
    "DELETE FROM prices WHERE instrument_id = 'FIGI_BBB' AND date > DATE '2020-08-31'"
  ))
  events <- query_fixture(con, paste(
    sql_silver_terminal_events("prices", last_day), "ORDER BY instrument_id"
  ))

  expect_equal(events$instrument_id, c("FIGI_AAA", "FIGI_BBB"))
  expect_equal(events$is_live, c(TRUE, FALSE))
  expect_equal(events$last_date[2], as.Date("2020-08-31"))
  expect_equal(events$max_gap_days[2], 6)
  expect_error(sql_silver_terminal_events("prices", "2020-09-30"), "Date")
})
