# Fixtures for the silver identity and price layer ----
#
# OpenFIGI replies, ordered by CUSIP:
#   AAA    a common stock priced every weekday, 10 USD, 1 million shares a day (liquid)
#   BBB    a 2 USD stock with a 5-day gap (illiquid by price)
#   ABAT   one ticker for two instruments (a dead and a live company): unattributable
#   BRK/B  share-class ticker, written BRK-B for Tiingo
#   AMBG   one CUSIP with two composite FIGIs: ambiguous, left out
#   an ETF, which is not a common stock
replies_fixture <- function() {
  tibble::tibble(
    cusip = c(
      "000000AA1", "000000BB2", "000000CC3", "000000DD4", "000000EE5", "000000EE5",
      "000000FF6"
    ),
    status = "found",
    composite_figi = c(
      "FIGI_AAA", "FIGI_BBB", "FIGI_ABAT1", "FIGI_ABAT2", "FIGI_AMB1", "FIGI_AMB2", "FIGI_ETF"
    ),
    share_class_figi = paste0("SC_", seq_len(7)),
    ticker = c("AAA", "BBB", "ABAT", "ABAT", "AMBG", "AMBG", "ETF1"),
    name = c("AAA CORP", "BBB INC", "ABAT OLD", "ABAT NEW", "AMB A", "AMB B", "AN ETF"),
    exch_code = "US",
    security_type = c(rep("Common Stock", 6), "ETP")
  ) |>
    dplyr::bind_rows(tibble::tibble(
      cusip = "000000GG7", status = "found", composite_figi = "FIGI_BRK",
      share_class_figi = "SC_8", ticker = "BRK/B", name = "BERKSHIRE B", exch_code = "US",
      security_type = "Common Stock"
    ))
}

## Weekdays from June to September 2020, with their position
prices_calendar_fixture <- function() {
  days <- seq(as.Date("2020-06-01"), as.Date("2020-09-30"), by = "day")
  days <- days[lubridate::wday(days, week_start = 1) <= 5]
  tibble::tibble(date = days, day_index = seq_along(days))
}

## Bronze Tiingo rows: AAA daily (one adjusted close of 0, one Saturday row), BBB with a
## gap, ABAT daily
bronze_prices_fixture <- function() {
  cal <- prices_calendar_fixture()$date
  aaa <- tibble::tibble(ticker = "AAA", date = cal, close = 10, volume = 1e6) |>
    dplyr::mutate(adj_close = dplyr::if_else(dplyr::row_number() == 10, 0, 10))
  bbb <- tibble::tibble(
    ticker = "BBB", date = cal[-(20:24)], close = 2, adj_close = 2, volume = 1e3
  )
  abat <- tibble::tibble(ticker = "ABAT", date = cal, close = 3, adj_close = 3, volume = 1e5)
  saturday <- tibble::tibble(
    ticker = "AAA", date = as.Date("2020-06-06"), close = 10, adj_close = 10, volume = 1
  )
  dplyr::bind_rows(aaa, bbb, abat, saturday) |>
    dplyr::mutate(status = "200", div_cash = 0, split_factor = 1)
}

## A duckdb connection holding the fixture tables
prices_con_fixture <- function(env = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  withr::defer(DBI::dbDisconnect(con), envir = env)
  duckdb::duckdb_register(con, "bronze", bronze_prices_fixture())
  duckdb::duckdb_register(con, "master", build_silver_security_master(replies_fixture()))
  duckdb::duckdb_register(con, "calendar", prices_calendar_fixture())
  DBI::dbExecute(con, paste(
    "CREATE TABLE prices AS", sql_silver_prices_d("bronze", "master", "calendar")
  ))
  con
}

query_fixture <- function(con, sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
