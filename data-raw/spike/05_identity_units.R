# G1 spike, step 3: identity chain on a stratified sample of about 60 CUSIPs
#
# Throwaway spike script. Needs spike.duckdb from 03_load_duckdb.R; calls OpenFIGI and
# Tiingo (keys from .Renviron, never printed).
# Output file: identity_sample.csv in the spike cache folder
#
# The identity chain, for each sampled stock:
#   1. CUSIP as reported in the 13F filings
#   2. OpenFIGI: CUSIP -> US composite FIGI (our instrument_id) and a ticker
#   3. Tiingo: ticker -> price series
#   4. Check: does Tiingo's price match the price implied by the filings?
#      implied price = VALUE x unit multiplier / number of shares, median over all filers
#      that held the stock, compared with Tiingo's unadjusted close on the last trading
#      day on or before the report date. A ratio near 1 proves the chain found the same
#      company the filers held.
# Unit multiplier (plan 1.5): 1000 if the filing date is before 2023-01-03, else 1.

## Fixed seed so the random sample is the same on every run
set.seed(13)
## x %||% y returns y when x is NULL (API replies omit absent fields); same as rlang's
`%||%` <- function(x, y) if (is.null(x)) y else x # nolint: object_name_linter.
out_dir <- "data-raw/cache/spike"
regime_cutoff <- as.Date("2023-01-03")

# Data from duckdb ----
db_path <- file.path(out_dir, "spike.duckdb")
if (!file.exists(db_path)) stop(db_path, " does not exist. Run 03_load_duckdb.R first.")
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE),
  dbdir = db_path,
  read_only = TRUE
)
## One query returns every share position with its filing's dates:
### JOIN links each holding row (INFOTABLE) to its filing (SUBMISSION)
### upper(CUSIP): some filers type CUSIPs in lower case (decision D2)
### WHERE keeps share positions only (SH), dropping options (PUTCALL) and bonds (PRN)
### strptime turns the text dates into real dates; TRY_CAST turns text into numbers
positions <- DBI::dbGetQuery(con, "
  SELECT i.archive, s.CIK AS cik, s.SUBMISSIONTYPE AS submission_type,
         strptime(s.FILING_DATE, '%d-%b-%Y')::DATE AS filing_date,
         strptime(s.PERIODOFREPORT, '%d-%b-%Y')::DATE AS period,
         upper(i.CUSIP) AS cusip, i.NAMEOFISSUER AS issuer,
         TRY_CAST(i.VALUE AS DOUBLE) AS value_raw,
         TRY_CAST(i.SSHPRNAMT AS DOUBLE) AS shares
  FROM INFOTABLE i JOIN SUBMISSION s USING (archive, ACCESSION_NUMBER)
  WHERE i.SSHPRNAMTTYPE = 'SH' AND i.PUTCALL IS NULL") |>
  tibble::as_tibble() |>
  dplyr::filter(.data$shares > 0, .data$value_raw > 0) |>
  dplyr::mutate(
    multiplier = dplyr::if_else(.data$filing_date < regime_cutoff, 1000, 1),
    implied_price = .data$value_raw * .data$multiplier / .data$shares
  )
DBI::dbDisconnect(con, shutdown = TRUE)

# Stratified identity sample ----
## Why a random, stratified sample: hand-picking stocks would bias the success rate.
## We split the end-2013 stocks into two groups (strata) and draw randomly from each:
### survivor      still held by some filer in 2025
### gone_by_2025  no longer held in 2025 (acquired, bankrupt, or CUSIP changed)
## The second group is where identity mapping is expected to fail, so it is oversampled.
## Base: the 1500 most widely held CUSIPs at end-2013 (original reports only)
held_2013 <- positions |>
  dplyr::filter(
    .data$archive == "2014q1_form13f.zip", .data$submission_type == "13F-HR",
    .data$period == as.Date("2013-12-31")
  ) |>
  dplyr::summarise(
    holders = dplyr::n_distinct(.data$cik), issuer = dplyr::first(.data$issuer),
    .by = "cusip"
  ) |>
  dplyr::slice_max(.data$holders, n = 1500, with_ties = FALSE)
## Survivor = held in a report FOR period 2025-03-31. The 2025 archive also carries late
## filings for old periods, which would wrongly mark long-gone stocks as survivors
## (Codex finding G1-01: FLIR, gone in 2021, appeared via a late filing).
cusips_2025 <- positions |>
  dplyr::filter(
    .data$archive == "01mar2025-31may2025_form13f.zip",
    .data$period == as.Date("2025-03-31")
  ) |>
  dplyr::distinct(.data$cusip)
held_2013 <- held_2013 |>
  dplyr::mutate(stratum = dplyr::if_else(.data$cusip %in% cusips_2025$cusip,
    "survivor", "gone_by_2025"
  ))
## Stratum sizes in the top 1500, needed to reweight the sample rates to the population
print(dplyr::count(held_2013, .data$stratum))
sample_random <- dplyr::bind_rows(
  held_2013 |> dplyr::filter(.data$stratum == "survivor") |> dplyr::slice_sample(n = 25),
  held_2013 |> dplyr::filter(.data$stratum == "gone_by_2025") |> dplyr::slice_sample(n = 30)
) |>
  dplyr::mutate(archive = "2014q1_form13f.zip", period = as.Date("2013-12-31"))

## Named special cases: known hard situations (dual share classes, renamed company,
## bankruptcies), taken from the data itself rather than typed from memory: most held
## CUSIP per issuer-name pattern in the 2023q1 archive
special_patterns <- c(
  alphabet = "^ALPHABET", berkshire = "^BERKSHIRE HATHAWAY",
  meta = "^META PLATFORMS|^FACEBOOK", twitter = "^TWITTER",
  svb = "^SVB FINL", bed_bath = "^BED BATH"
)
sample_special <- purrr::imap(special_patterns, function(pattern, label) {
  positions |>
    dplyr::filter(
      .data$archive == "2023q1_form13f.zip",
      .data$period == as.Date("2022-12-31"),
      stringr::str_detect(.data$issuer, pattern)
    ) |>
    dplyr::summarise(
      holders = dplyr::n_distinct(.data$cik),
      issuer = dplyr::first(.data$issuer), .by = "cusip"
    ) |>
    dplyr::filter(.data$holders >= 20) |>
    dplyr::mutate(stratum = paste0("special_", label))
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(archive = "2023q1_form13f.zip", period = as.Date("2022-12-31"))
## A special case already drawn in the random sample is kept once (in its random stratum)
sample_special <- dplyr::anti_join(sample_special, sample_random, by = dplyr::join_by(cusip))
id_sample <- dplyr::bind_rows(sample_random, sample_special)
cat("identity sample:", nrow(id_sample), "CUSIPs\n")
print(dplyr::count(id_sample, .data$stratum))

# OpenFIGI mapping ----
## One POST request carries up to 100 "jobs" (one per CUSIP). Per job:
### exchCode = US                  return only the US listing, not the ~500 listings of
###                                the same share on exchanges worldwide
### includeUnlistedEquities = TRUE also return delisted companies (needed for dead names)
## The answer per job is either data (one or more records) or a warning such as
## "No identifier found."
map_openfigi <- function(cusips) {
  jobs <- purrr::map(cusips, function(x) {
    ### A CUSIP starting with a letter is a CINS (issuer incorporated outside the US);
    ### OpenFIGI only finds it with idType ID_CINS (decision D2)
    id_type <- if (stringr::str_detect(x, "^[A-Z]")) "ID_CINS" else "ID_CUSIP"
    list(idType = id_type, idValue = x, exchCode = "US", includeUnlistedEquities = TRUE)
  })
  resp <- httr2::request("https://api.openfigi.com/v3/mapping") |>
    httr2::req_headers(`X-OPENFIGI-APIKEY` = Sys.getenv("OPENFIGI_API_KEY")) |>
    httr2::req_body_json(jobs) |>
    httr2::req_throttle(rate = 4, realm = "openfigi") |>
    httr2::req_perform()
  body <- httr2::resp_body_json(resp)
  purrr::map2(cusips, body, function(cusip, res) {
    if (is.null(res$data)) {
      return(tibble::tibble(cusip = cusip, figi_status = res$warning %||% "error"))
    }
    recs <- purrr::map(res$data, function(r) {
      tibble::tibble(
        figi = r$figi %||% NA, composite_figi = r$compositeFIGI %||% NA,
        ticker = r$ticker %||% NA, figi_name = r$name %||% NA,
        security_type = r$securityType %||% NA,
        security_type2 = r$securityType2 %||% NA
      )
    }) |>
      purrr::list_rbind()
    ## The composite-level record is the one whose FIGI equals its composite FIGI
    ## (falls back to the first record when none is composite-level).
    ## The lint exclusion below is needed because lintr cannot see rlang's .data pronoun
    ## inside functions in scripts outside the package; dplyr resolves it at run time.
    best <- dplyr::filter(recs, .data$figi == .data$composite_figi) # nolint: object_usage_linter.
    if (nrow(best) == 0) best <- recs
    best <- dplyr::slice(best, 1)
    dplyr::mutate(best, cusip = cusip, figi_status = "ok", n_records = nrow(recs))
  }) |>
    purrr::list_rbind()
}
figi_map <- map_openfigi(id_sample$cusip)

# Tiingo metadata and prices ----
## Two Tiingo calls per ticker:
### tiingo/daily/{ticker}         metadata: company name, first and last price date
### tiingo/daily/{ticker}/prices  daily prices; we ask only for the 10 days before the
###                               report date and keep the last close
## req_error(is_error = FALSE) lets a 404 (unknown ticker) come back as a status code
## instead of stopping the script, so it can be counted as a failure.
tiingo_get <- function(path, query = list()) {
  resp <- httr2::request("https://api.tiingo.com") |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_headers(
      Authorization = paste("Token", Sys.getenv("TIINGO_API_KEY")),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_throttle(rate = 2, realm = "tiingo") |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  list(
    status = httr2::resp_status(resp),
    body = if (httr2::resp_status(resp) == 200) {
      httr2::resp_body_json(resp, simplifyVector = TRUE)
    }
  )
}
## OpenFIGI writes share classes as BRK/B; Tiingo uses BRK-B
tiingo_ticker <- function(x) stringr::str_replace_all(x, "/", "-")

tiingo_check <- function(ticker, period) {
  if (is.na(ticker)) {
    return(tibble::tibble(meta_status = NA_integer_))
  }
  tk <- tiingo_ticker(ticker)
  meta <- tiingo_get(paste0("tiingo/daily/", tk))
  px <- tiingo_get(
    paste0("tiingo/daily/", tk, "/prices"),
    list(startDate = format(period - 10), endDate = format(period))
  )
  close_at_period <- if (px$status == 200 && length(px$body) > 0) {
    utils::tail(px$body$close, 1)
  } else {
    NA_real_
  }
  tibble::tibble(
    meta_status = meta$status,
    tiingo_name = meta$body$name %||% NA, tiingo_start = meta$body$startDate %||% NA,
    tiingo_end = meta$body$endDate %||% NA, tiingo_exchange = meta$body$exchangeCode %||% NA,
    price_status = px$status, close_at_period = close_at_period
  )
}

# Implied price per sampled CUSIP (median over original reports for that period) ----
## The median across filers is robust to the few filers who report a wrong value
implied <- positions |>
  dplyr::filter(.data$submission_type == "13F-HR") |>
  dplyr::semi_join(id_sample, by = dplyr::join_by(cusip, archive, period)) |>
  dplyr::summarise(
    implied_price = stats::median(.data$implied_price),
    .by = c("cusip", "archive", "period")
  )

## Assemble the chain per stock and decide success:
### chain_ok = FIGI found AND Tiingo price found AND price ratio between 0.8 and 1.25
identity <- id_sample |>
  dplyr::left_join(figi_map, by = dplyr::join_by(cusip), relationship = "one-to-one") |>
  dplyr::left_join(implied,
    by = dplyr::join_by(cusip, archive, period),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(tiingo = purrr::map2(.data$ticker, .data$period, tiingo_check)) |>
  tidyr::unnest("tiingo") |>
  dplyr::mutate(
    price_ratio = .data$implied_price / .data$close_at_period,
    chain_ok = .data$figi_status == "ok" & !is.na(.data$close_at_period) &
      dplyr::between(.data$price_ratio, 0.8, 1.25)
  )
readr::write_csv(identity, file.path(out_dir, "identity_sample.csv"))

cat("\n=== Identity chain success by stratum ===\n")
print(identity |>
  dplyr::mutate(stratum = dplyr::if_else(stringr::str_starts(.data$stratum, "special"),
    "special", .data$stratum
  )) |>
  dplyr::summarise(
    n = dplyr::n(), figi_found = sum(.data$figi_status == "ok"),
    tiingo_price_found = sum(!is.na(.data$close_at_period)),
    chain_ok = sum(.data$chain_ok), .by = "stratum"
  ))
cat("\n=== Per CUSIP ===\n")
print(
  identity |>
    dplyr::select(
      "stratum", "cusip", "issuer", "ticker", "composite_figi",
      "security_type", "tiingo_name", "tiingo_end", "price_ratio",
      "chain_ok"
    ),
  n = Inf, width = Inf
)
