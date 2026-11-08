## Eight candidates of the 2019-08-30 formation, one per exclusion reason
universe_inputs <- function() {
  ids <- paste0("FIGI_", 1:8)
  breadth <- tibble::tibble(
    formation_date = as.Date("2019-08-30"), instrument_id = ids,
    n_prev = c(30, 30, 30, 30, 30, 1, 30, 0),
    n_curr = c(40, 40, 40, 40, 40, 30, 40, 10),
    n_init = c(12, 12, 12, 12, 12, 29, 12, 10),
    n_exit = c(2, 2, 2, 2, 2, 0, 2, 0),
    n_match = c(40, 40, 40, 40, 10, 30, 40, 10)
  )
  features <- tibble::tibble(
    instrument_id = ids, month = as.Date("2019-08-01"), date = as.Date("2019-08-30"),
    close_prev = c(20, 20, 20, 20, 20, 20, 3, 20),
    median_volfin_3m = c(9e6, 8e6, 7e6, 6e6, 5e6, 4e6, 3e6, 1e7),
    mean_volfin_3m = 1e7, presence = 100, daily_vol = 1, vol_3m = 1, mom_12_1 = 5,
    n_mom = 230, rev_1m = 1, amihud = 0.01
  )
  master <- tibble::tibble(
    cusip = paste0("C", 1:8), instrument_id = ids,
    ticker = c(paste0("T", 1:3), NA, paste0("T", 5:8)),
    has_price_source = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
  terminal <- tibble::tibble(instrument_id = ids, first_date = as.Date("2015-01-02"))
  list(breadth = breadth, features = features, master = master, terminal = terminal)
}

build_universe_fixture <- function(n_universe = 2) {
  x <- universe_inputs()
  build_gold_universe(gold_formations(), x$breadth, x$features, x$terminal, x$master,
    n_universe = n_universe
  )
}

test_that("each exclusion rule removes its candidate, in the plan's order", {
  universe <- build_universe_fixture()
  expect_equal(universe$excluded_reason, c(
    NA, "below_liquidity_rank", "below_liquidity_rank", "no_price_source",
    "identity_rejected", "cusip_change_suspect", "price_below_min", "zero_holders"
  ))
})

test_that("zero-holder stocks leave after the liquidity ranking (D5)", {
  ## FIGI_8 ranks first by dollar volume, so only one of the top two remains
  universe <- build_universe_fixture()
  expect_equal(universe$liquidity_rank[universe$instrument_id == "FIGI_8"], 1L)
  expect_equal(universe$instrument_id[universe$in_universe], "FIGI_1")
})

test_that("a stock not traded at t or with a short history is excluded", {
  x <- universe_inputs()
  features <- dplyr::mutate(x$features, date = dplyr::if_else(
    .data$instrument_id == "FIGI_1", as.Date("2019-08-29"), .data$date
  ))
  terminal <- dplyr::mutate(x$terminal, first_date = dplyr::if_else(
    .data$instrument_id == "FIGI_2", as.Date("2019-01-02"), .data$first_date
  ))
  universe <- build_gold_universe(gold_formations(), x$breadth, features, terminal, x$master)
  expect_equal(universe$excluded_reason[1:2], c("not_traded_at_t", "short_history"))
})

test_that("a stale stock traded on under 90% of recent days is excluded before ranking", {
  ## FIGI_1 still has a price every day but traded on 47 of the previous 63 days (a
  ## frozen price after a cash takeover); FIGI_2 traded on 57 of 63 days and passes
  x <- universe_inputs()
  features <- dplyr::mutate(x$features, presence = dplyr::case_when(
    .data$instrument_id == "FIGI_1" ~ 100 * 47 / 63,
    .data$instrument_id == "FIGI_2" ~ 100 * 57 / 63,
    .default = .data$presence
  ))
  universe <- build_gold_universe(gold_formations(), x$breadth, features, x$terminal, x$master,
    n_universe = 2
  )
  expect_equal(universe$excluded_reason[1:2], c("low_presence", NA))
  expect_true(is.na(universe$liquidity_rank[1]))
})
