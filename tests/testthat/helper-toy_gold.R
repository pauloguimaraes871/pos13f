# Gold toy fixture: a reproducible sample of real silver data ----
#
# Built by data-raw/make_toy_gold_fixture.R: 22 instruments over the formations of
# 2019-11-29, 2020-02-28 and 2020-05-29 (16 drawn at random with a fixed seed, 6 forced
# edge cases; see toy$selected$reason), 400 filers drawn at random, their holdings
# versions, prices and returns.
toy_silver_gold <- function() {
  readRDS(testthat::test_path("testdata", "toy_silver_gold.rds"))
}

## The first day of the toy's unfinished month: the toy prices end on 2020-09-30
toy_gold_last_month <- as.Date("2020-09-01")

## A duckdb connection holding the toy tables under their silver names
toy_gold_con <- function(toy = toy_silver_gold(), env = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  withr::defer(DBI::dbDisconnect(con), envir = env)
  tables <- list(
    silver_holdings_state = toy$state, silver_prices_d = toy$prices_d,
    silver_returns_d = toy$returns_d, formations = toy$formations, cohort = toy$cohort,
    master = toy$master
  )
  purrr::iwalk(tables, \(df, name) duckdb::duckdb_register(con, name, df))
  con
}

## The SQL result of a gold builder on the toy tables
toy_gold_sql <- function(con, sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))

## The toy run end to end: universe (8 members, 300 days of history), panels, the
## sampled dplyr reference, and the inputs the gold suite reads. The toy holds three
## formations, so its last holding month (August 2020) is treated as unfinished.
toy_gold_build <- function(env = parent.frame()) {
  toy <- toy_silver_gold()
  con <- toy_gold_con(toy, env)
  breadth <- toy_gold_sql(con, sql_gold_breadth(
    "silver_holdings_state", "formations", "cohort", "master", "silver_prices_d"
  ))
  features <- toy_gold_sql(con, sql_gold_monthly_features("silver_prices_d", "silver_returns_d"))
  monthly <- toy_gold_sql(con, sql_gold_monthly_returns("silver_returns_d"))
  universe <- build_gold_universe(toy$formations, breadth, features, toy$terminal, toy$master,
    n_universe = 8, min_history_days = 300
  )
  panels <- build_gold_panels(toy$formations, universe, toy$cohort, features, monthly,
    toy$terminal,
    last_calendar_day = as.Date("2020-08-31")
  )
  reference <- build_gold_reference(con,
    sample_gold_check_rows(panels, toy$terminal, n_per_stratum = 4), toy$formations, toy$cohort,
    toy$master, toy$terminal, identity,
    last_month = as.Date("2020-08-01"),
    activity_pool = dplyr::filter(toy$cohort, .data$cik %in% toy$full_state_ciks)
  )
  list(toy = toy, panels = panels, reference = reference)
}

## Run one gold suite block on the toy build; TRUE when every expectation passes
toy_gold_block_passes <- function(block, env = parent.frame()) {
  suppressMessages(run_logged_test(block,
    log_file = "test_gold_block", log_path = withr::local_tempdir(.local_envir = env)
  ))
}
