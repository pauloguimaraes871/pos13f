# G1 spike, steps 2 and 4: row-level unit test, Tiingo ticker list
#
# Throwaway spike script. Needs spike.duckdb (03) and identity_sample.csv (05).
# Two follow-ups to script 05 (the CINS retry that used to be here now lives in 05):
#   2. Unit test on every holding row of the stocks whose chain succeeded: is VALUE in
#      thousands or in dollars? Compared under two rules, the filing-date rule (plan 1.5)
#      and a report-period rule, to show which one matches real prices.
#   3. Tiingo's list of all supported tickers: delisted coverage and reused tickers.

out_dir <- "data-raw/cache/spike"
regime_cutoff <- as.Date("2023-01-03")
identity <- readr::read_csv(file.path(out_dir, "identity_sample.csv"), show_col_types = FALSE)

# 2. Row-level unit test on the CUSIPs whose chain succeeded ----
## For each holding row: ratio = VALUE x multiplier / shares / Tiingo close.
## Near 1 means the unit was read correctly; near 1000 or 0.001 means the filer used the
## other unit. Rows are split into three groups:
### a  filed before 2023-01-03 (thousands expected)
### b  filed after, for the current period (dollars expected)
### c  filed after, for an OLD period (the case that separates the two rules: the
###    filing-date rule expects dollars, the period rule expects thousands)
## duckdb_register lets the database see an R data frame (the CUSIP list) as a table
ok <- identity |>
  dplyr::filter(.data$chain_ok) |>
  dplyr::distinct(.data$cusip, .data$ticker)
db_path <- file.path(out_dir, "spike.duckdb")
if (!file.exists(db_path)) stop(db_path, " does not exist. Run 03_load_duckdb.R first.")
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE),
  dbdir = db_path,
  read_only = TRUE
)
duckdb::duckdb_register(con, "ok_cusips", ok)
rows <- DBI::dbGetQuery(con, "
  SELECT s.SUBMISSIONTYPE AS submission_type,
         strptime(s.FILING_DATE, '%d-%b-%Y')::DATE AS filing_date,
         strptime(s.PERIODOFREPORT, '%d-%b-%Y')::DATE AS period,
         upper(i.CUSIP) AS cusip, TRY_CAST(i.VALUE AS DOUBLE) AS value_raw,
         TRY_CAST(i.SSHPRNAMT AS DOUBLE) AS shares
  FROM INFOTABLE i JOIN SUBMISSION s USING (archive, ACCESSION_NUMBER)
  WHERE i.SSHPRNAMTTYPE = 'SH' AND i.PUTCALL IS NULL
    AND upper(i.CUSIP) IN (SELECT cusip FROM ok_cusips)") |>
  tibble::as_tibble() |>
  dplyr::filter(.data$shares > 0, .data$value_raw > 0)
DBI::dbDisconnect(con, shutdown = TRUE)

## One Tiingo request per ticker covers every period in the sample
fetch_closes <- function(ticker) {
  resp <- httr2::request("https://api.tiingo.com") |>
    httr2::req_url_path_append(
      "tiingo/daily", stringr::str_replace_all(ticker, "/", "-"),
      "prices"
    ) |>
    httr2::req_url_query(startDate = "2008-12-01", endDate = "2025-04-15") |>
    httr2::req_headers(Authorization = paste("Token", Sys.getenv("TIINGO_API_KEY"))) |>
    httr2::req_throttle(rate = 2, realm = "tiingo") |>
    httr2::req_perform()
  px <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  tibble::tibble(ticker = ticker, date = as.Date(substr(px$date, 1, 10)), close = px$close)
}
closes <- purrr::map(unique(ok$ticker), fetch_closes) |> purrr::list_rbind()

## Close on the last trading day on or before each period end (periods can fall on weekends)
### join_by(closest(period >= date)) picks, per period, the latest price date not after it;
### the 5-day filter rejects a match when the stock had no recent price
period_close <- rows |>
  dplyr::distinct(.data$cusip, .data$period) |>
  dplyr::inner_join(ok, by = dplyr::join_by(cusip), relationship = "many-to-one") |>
  dplyr::inner_join(closes,
    by = dplyr::join_by(ticker, closest(period >= date)),
    relationship = "many-to-one"
  ) |>
  dplyr::filter(as.numeric(.data$period - .data$date) <= 5) |>
  dplyr::select("cusip", "period", "close")

unit_test <- rows |>
  dplyr::inner_join(period_close,
    by = dplyr::join_by(cusip, period),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    group = dplyr::case_when(
      .data$filing_date < regime_cutoff ~ "a_filed_before_2023-01-03",
      .data$period >= as.Date("2022-12-31") ~ "b_filed_after_current_period",
      TRUE ~ "c_filed_after_old_period"
    ),
    ratio_filing_rule = .data$value_raw *
      dplyr::if_else(.data$filing_date < regime_cutoff, 1000, 1) / .data$shares / .data$close,
    ratio_period_rule = .data$value_raw *
      dplyr::if_else(.data$period < regime_cutoff, 1000, 1) / .data$shares / .data$close
  )
band <- function(r) {
  dplyr::case_when(
    dplyr::between(r, 0.8, 1.25) ~ "near_1",
    dplyr::between(r, 500, 2000) ~ "near_1000",
    dplyr::between(r, 5e-4, 2e-3) ~ "near_0.001",
    TRUE ~ "other"
  )
}
cat("\n=== Unit test: share of rows per ratio band, filing-date rule vs period rule ===\n")
print(unit_test |>
  dplyr::mutate(
    filing_rule = band(.data$ratio_filing_rule),
    period_rule = band(.data$ratio_period_rule)
  ) |>
  tidyr::pivot_longer(c("filing_rule", "period_rule"),
    names_to = "rule",
    values_to = "band"
  ) |>
  dplyr::count(.data$group, .data$rule, .data$band) |>
  dplyr::mutate(
    share = round(.data$n / sum(.data$n), 4),
    .by = c("group", "rule")
  ) |>
  dplyr::arrange(.data$group, .data$rule, dplyr::desc(.data$n)), n = Inf)
cat(
  "old-period rows filed after 2023-01-03 (group c): accessions =",
  nrow(dplyr::distinct(
    dplyr::filter(unit_test, .data$group == "c_filed_after_old_period"),
    .data$filing_date, .data$period
  )), "\n"
)

# 3. Tiingo supported tickers: delisted coverage and ticker reuse ----
## Tiingo publishes one file listing every ticker it covers, with first and last price
## dates. A ticker listed twice was used by two different companies at different times.
zip_path <- file.path(out_dir, "supported_tickers.zip")
httr2::request("https://apimedia.tiingo.com/docs/tiingo/daily/supported_tickers.zip") |>
  httr2::req_perform(path = zip_path)
tickers <- readr::read_csv(zip_path, show_col_types = FALSE)
cat("\n=== Tiingo supported tickers ===\n")
cat("columns:", paste(names(tickers), collapse = ", "), "\n")
print(dplyr::count(tickers, .data$assetType, .data$exchange, sort = TRUE) |> utils::head(12))
is_delisted_us <- tickers$assetType == "Stock" & tickers$priceCurrency == "USD" &
  tickers$endDate < as.Date("2026-01-01")
cat("US stocks with an end date before 2026 (delisted):", sum(is_delisted_us, na.rm = TRUE), "\n")
cat(
  "tickers appearing more than once (reuse):",
  sum(duplicated(tickers$ticker)), "\n"
)
print(tickers |> dplyr::filter(.data$ticker %in% c("STR", "LPS", "TWTR", "SIVB", "SIVBQ")))
