# Build the gold toy fixture: a reproducible sample of real silver data ----
#
# Same idea as make_toy_fixtures.R (factoRverse `toy_*` objects are real samples): the
# gold unit tests run the SQL builders and the dplyr references on real rows, with the
# noise of real prices and holdings (gaps, amendments, splits, dead stocks).
#
# Scope: three consecutive formations (2019-11-29, 2020-02-28, 2020-05-29), so the
# sample spans the March 2020 crash.
#   random  16 universe members of the 2020-02-28 formation, drawn with a fixed seed
#   forced  for each edge case, one instrument drawn at random among those that have it:
#           a stock that stopped trading in a holding month (terminal stress), a
#           suspected CUSIP change (D9), a stock below 5 USD at one formation and a member
#           at another, a priced candidate with trading gaps
#           (presence below 100), an identity
#           rejection, and the member with the fewest quarter-q holders
#   filers  300 cohort members and 100 excluded filers of these formations, drawn at
#           random, with every holdings version they filed for the instruments' CUSIPs;
#           the cohort table itself is complete, with its filings, holdings versions
#           and relations; 6 cohort members keep their complete holdings
#
# Output: tests/testthat/testdata/toy_silver_gold.rds, a list with `formations`,
# `cohort`, `filings`, `versions`, `relations`, `master`, `terminal`, `state`, `prices_d`,
# `returns_d` and `selected`
# (instrument and reason). Rerun only when silver or gold change:
#   Rscript data-raw/make_toy_gold_fixture.R

devtools::load_all(quiet = TRUE)
board <- init_pipeline_board(file.path("data", "prod"))
read <- function(name) pins::pin_read(board, name)
set.seed(20261108)

# Formations and candidates ----
formations <- build_formation_dates(read("silver_trading_calendar")$date) |>
  dplyr::filter(.data$formation_date %in% as.Date(c("2019-11-29", "2020-02-28", "2020-05-29")))
universe <- read("gold_universe_diagnostics") |>
  dplyr::filter(.data$formation_date %in% formations$formation_date)
terminal <- read("silver_terminal_events")

# Instruments ----
draw <- function(ids, reason) {
  ids <- unique(ids)
  tibble::tibble(instrument_id = ids[sample.int(length(ids), 1)], reason = reason)
}
members_feb <- universe |>
  dplyr::filter(.data$in_universe, .data$formation_date == as.Date("2020-02-28"))
ended <- terminal |>
  dplyr::filter(
    !.data$is_live,
    .data$last_date >= as.Date("2019-12-01"), .data$last_date <= as.Date("2020-08-31")
  ) |>
  dplyr::semi_join(dplyr::filter(universe, .data$in_universe), by = dplyr::join_by("instrument_id"))
reasons <- function(r) dplyr::filter(universe, .data$excluded_reason == r)$instrument_id
ever_member <- dplyr::filter(universe, .data$in_universe)$instrument_id
crossed <- intersect(reasons("price_below_min"), ever_member)
## Members almost always trade daily, so gaps are drawn among priced candidates
gappy <- dplyr::filter(universe, .data$presence < 100, !is.na(.data$ticker))
selected <- dplyr::bind_rows(
  tibble::tibble(
    instrument_id = sample(members_feb$instrument_id, 16), reason = "random member"
  ),
  draw(ended$instrument_id, "stopped trading in a holding month"),
  draw(reasons("cusip_change_suspect"), "suspected CUSIP change"),
  draw(crossed, "below 5 USD at one formation, member at another"),
  draw(gappy$instrument_id, "candidate with trading gaps"),
  draw(reasons("identity_rejected"), "identity rejected"),
  tibble::tibble(
    instrument_id =
      dplyr::slice_min(members_feb, .data$n_curr, n = 1, with_ties = FALSE)$instrument_id,
    reason = "fewest quarter-q holders"
  )
) |>
  dplyr::distinct(.data$instrument_id, .keep_all = TRUE)

# Filers ----
cohort_all <- read("gold_cohort") |>
  dplyr::filter(.data$formation_date %in% formations$formation_date)
pick_ciks <- function(flag, n) {
  ciks <- unique(cohort_all$cik[cohort_all$in_cohort == flag])
  sample(ciks, min(n, length(ciks)))
}
ciks <- c(pick_ciks(TRUE, 300), pick_ciks(FALSE, 100))
## Six cohort members with at most 300 positions keep their complete holdings, so the
## activity counts of the cohort table can be recomputed on the toy
full_state_ciks <- cohort_all |>
  dplyr::filter(.data$in_cohort, .data$cik %in% ciks, .data$n_curr <= 300) |>
  dplyr::distinct(.data$cik) |>
  dplyr::slice_sample(n = 6) |>
  dplyr::pull("cik")
## The whole cohort is kept (the index-like terciles are computed over every filer), with
## the filings and inclusion relations its suite checks re-read
cohort <- cohort_all
quarters <- unique(c(formations$report_quarter, formations$prev_quarter))
filings <- read("silver_filings") |>
  dplyr::filter(.data$cik %in% cohort$cik, .data$period_of_report %in% quarters) |>
  dplyr::select("accession_number", "cik", "period_of_report", "usable_date")
versions <- read("silver_holdings_versions") |>
  dplyr::filter(.data$cik %in% cohort$cik, .data$period_of_report %in% quarters)
relations <- read("silver_filing_relations") |>
  dplyr::filter(
    .data$relation == "included_in_report", .data$accession_number %in% filings$accession_number
  )

# Silver rows of the selection, inside duckdb ----
master <- read("silver_security_master") |>
  dplyr::filter(.data$instrument_id %in% selected$instrument_id)
con <- open_duckdb_work("3GB", file.path("data-raw", "cache", "duckdb_tmp"))
rel <- function(name) {
  paste0("read_parquet('", normalizePath(pins::pin_download(board, name), winslash = "/"), "')")
}
duckdb::duckdb_register(con, "sel_cusip", dplyr::distinct(master, .data$cusip))
duckdb::duckdb_register(con, "sel_cik", tibble::tibble(cik = ciks))
duckdb::duckdb_register(con, "sel_inst", dplyr::distinct(selected, .data$instrument_id))
report_quarters <- unique(c(formations$report_quarter, formations$prev_quarter))
periods <- paste0("'", report_quarters, "'", collapse = ", ")
get <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
state <- get(paste0(
  "SELECT h.* FROM ", rel("silver_holdings_state"), " h JOIN sel_cusip USING (cusip) ",
  "JOIN sel_cik USING (cik) WHERE h.period_of_report IN (", periods, ")"
))
duckdb::duckdb_register(con, "full_cik", tibble::tibble(cik = full_state_ciks))
state <- dplyr::bind_rows(state, get(paste0(
  "SELECT h.* FROM ", rel("silver_holdings_state"), " h JOIN full_cik USING (cik) ",
  "WHERE h.period_of_report IN (", periods, ")"
))) |>
  dplyr::distinct()
window <- "BETWEEN DATE '2018-06-01' AND DATE '2020-09-30'"
prices_d <- get(paste0(
  "SELECT p.* FROM ", rel("silver_prices_d"), " p JOIN sel_inst USING (instrument_id) ",
  "WHERE p.date ", window
))
returns_d <- get(paste0(
  "SELECT r.* FROM ", rel("silver_returns_d"), " r JOIN sel_inst USING (instrument_id) ",
  "WHERE r.date ", window
))
DBI::dbDisconnect(con)

toy <- list(
  formations = formations, cohort = cohort, filings = filings, versions = versions,
  relations = relations,
  master = master,
  terminal = dplyr::filter(terminal, .data$instrument_id %in% selected$instrument_id),
  state = state, full_state_ciks = full_state_ciks, prices_d = prices_d,
  returns_d = returns_d, selected = selected
)
path <- file.path("tests", "testthat", "testdata", "toy_silver_gold.rds")
saveRDS(toy, path, compress = "xz")
message(
  "Wrote ", path, ": ", nrow(selected), " instruments, ", length(ciks), " filers, ",
  nrow(state), " holdings rows, ", nrow(prices_d), " price rows, ",
  round(file.size(path) / 1e6, 2), " MB"
)
