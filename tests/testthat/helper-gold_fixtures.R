# Fixtures for the gold layer ----
#
# One formation, 2019-08-30, reading report quarters 2019-03-31 (q-1) and 2019-06-30
# (q). Six filers, ordered by CIK:
#   0000000001  both reports public before t, 12 positions, low turnover, many positions
#   0000000002  its q report becomes usable only after t: not a cohort candidate
#   0000000003  3 positions: too few
#   0000000004  12 positions, high turnover
#   0000000005  10 positions, medium turnover; included in filer 4's q report (overlap)
#   0000000006  11 positions, high turnover
gold_calendar <- function() {
  days <- seq(as.Date("2019-01-01"), as.Date("2020-02-28"), by = "day")
  days[lubridate::wday(days, week_start = 1) <= 5]
}

gold_formations <- function() {
  build_formation_dates(gold_calendar(), first_quarter = as.Date("2019-03-31")) |>
    dplyr::filter(.data$formation_date == as.Date("2019-08-30"))
}

gold_versions <- function() {
  cik <- sprintf("%010d", c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6))
  tibble::tibble(
    cik = cik,
    period_of_report = as.Date(rep(c("2019-03-31", "2019-06-30"), 6)),
    accession_number = paste0(cik, "-19-0000", rep(1:2, 6)),
    valid_from = as.Date(c(
      "2019-05-10", "2019-08-10", "2019-05-10", "2019-09-05", rep(c("2019-05-10", "2019-08-10"), 4)
    )),
    valid_to = as.Date(NA),
    is_visible = TRUE, kind = "replace", event_no = 1L, base_no = 1L
  ) |>
    dplyr::mutate(usable_date = .data$valid_from)
}

gold_activity <- function() {
  tibble::tibble(
    formation_date = as.Date("2019-08-30"),
    cik = sprintf("%010d", c(1, 3, 4, 5, 6)),
    n_prev = c(12, 3, 12, 10, 11),
    n_curr = c(12, 3, 12, 10, 11),
    n_both = c(12, 3, 4, 8, 3),
    value_curr = c(9e8, 1e7, 5e8, 3e8, 2e8)
  )
}

gold_relations <- function() {
  tibble::tibble(
    accession_number = "0000000004-19-00002", relation = "included_in_report",
    sequence_number = 1, related_cik = "0000000005", related_file_number = NA_character_,
    related_name = "FIVE ADVISERS"
  )
}

gold_filings <- function() {
  v <- gold_versions()
  tibble::tibble(
    accession_number = v$accession_number, cik = v$cik,
    period_of_report = v$period_of_report, usable_date = v$valid_from
  )
}

gold_cohort_fixture <- function() {
  select_gold_cohort(gold_formations(), gold_versions(), gold_activity(),
    relations = gold_relations()
  )
}

## Panels of the 2019-08-30 formation for two members; the calendar ends in November
## 2019, so November is the unfinished month
panels_fixture <- function(terminal = NULL) {
  universe <- tibble::tibble(
    formation_date = as.Date("2019-08-30"), instrument_id = c("FIGI_B", "FIGI_A"),
    n_prev = c(40, 30), n_curr = c(44, 27), n_init = c(6, 1), n_exit = c(2, 4),
    mom_12_1 = c(10, -5), rev_1m = c(1, 2), vol_3m = c(1.5, 2.5), amihud = c(0.01, 0.02),
    in_universe = TRUE
  )
  cohort <- tibble::tibble(formation_date = as.Date("2019-08-30"), in_cohort = rep(TRUE, 100))
  features <- tibble::tibble(
    instrument_id = rep(c("FIGI_A", "FIGI_B"), each = 2),
    month = as.Date(rep(c("2019-08-01", "2019-09-01"), 2)),
    mean_volfin_3m = c(1e7, 1.1e7, 2e7, 2.1e7), median_volfin_3m = c(9e6, 1e7, 1.9e7, 2e7),
    presence = 100, close_prev = c(20, 21, 30, 31), daily_vol = c(1, NA, 2, 2.2),
    vol_3m = c(1.2, 1.3, 2.4, 2.5)
  )
  ## FIGI_A has no price in October: its October features carry September's
  monthly_returns <- tibble::tibble(
    instrument_id = c("FIGI_A", "FIGI_A", "FIGI_B", "FIGI_B", "FIGI_B"),
    month = as.Date(c("2019-09-01", "2019-10-01", "2019-09-01", "2019-10-01", "2019-11-01")),
    ret_m = c(0.02, -0.01, 0.05, 0.03, 0.01)
  )
  if (is.null(terminal)) {
    terminal <- tibble::tibble(
      instrument_id = c("FIGI_A", "FIGI_B"), last_date = as.Date("2019-11-15"), is_live = TRUE
    )
  }
  formations <- build_formation_dates(gold_calendar(), first_quarter = as.Date("2019-03-31")) |>
    dplyr::filter(.data$formation_date == as.Date("2019-08-30"))
  build_gold_panels(formations, universe, cohort, features, monthly_returns, terminal,
    last_calendar_day = as.Date("2019-11-15")
  )
}
