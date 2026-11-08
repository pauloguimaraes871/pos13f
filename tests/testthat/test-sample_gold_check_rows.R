## Unbalanced panels of 40 instruments over six months: FIGI_03 stops trading in
## 2019-10, and every seventh instrument leaves the universe at the December formation
sample_panels <- function() {
  ids <- sprintf("FIGI_%02d", 1:40)
  dates <- seq(as.Date("2019-09-01"), as.Date("2020-02-01"), by = "month")
  signals <- tibble::tibble(tickers = rep(ids, each = 6), dates = rep(dates, 40)) |>
    dplyr::filter(
      !(.data$tickers == "FIGI_03" & .data$dates > as.Date("2019-11-01")),
      !(readr::parse_number(.data$tickers) %% 7 == 0 & .data$dates >= as.Date("2019-12-01"))
    ) |>
    dplyr::mutate(
      id = paste0(.data$tickers, "-", .data$dates),
      signal_age_months = (lubridate::month(.data$dates) - 9) %% 3,
      n_holders_curr = as.numeric(readr::parse_number(.data$tickers))
    ) |>
    dplyr::relocate("id") |>
    dplyr::arrange(.data$id)
  returns <- dplyr::transmute(signals, .data$id,
    fwd_return_1m = dplyr::if_else(.data$dates == max(.data$dates), NA_real_, 1)
  )
  list(signals_m_df = signals, fwd_1m_returns_m_df = returns)
}
sample_terminal <- function() {
  tibble::tibble(
    instrument_id = sprintf("FIGI_%02d", 1:40),
    last_date = dplyr::if_else(1:40 == 3, as.Date("2019-10-15"), as.Date("2020-02-14")),
    is_live = 1:40 != 3
  )
}

test_that("every stratum is drawn, from finished rows, and each row once", {
  s <- sample_gold_check_rows(sample_panels(), sample_terminal(), n_per_stratum = 5)
  expect_setequal(
    unique(s$stratum),
    c("regular", "terminal_month", "first_formation", "few_holders", "old_signal")
  )
  expect_false(anyDuplicated(s$id) > 0)
  expect_false(any(s$dates == as.Date("2020-02-01")))
  expect_equal(s$id[s$stratum == "terminal_month"], "FIGI_03-2019-10-01")
  expect_true(all(s$signal_age_months[s$stratum == "old_signal"] == 2))
  expect_true(all(s$dates[s$stratum == "first_formation"] == as.Date("2019-09-01")))
  ## Every stratum with rows is available and represented by a flagged row
  expect_setequal(
    attr(s, "available_strata"),
    c("regular", "terminal_month", "first_formation", "few_holders", "old_signal")
  )
  expect_true(all(purrr::map_lgl(attr(s, "available_strata"), \(k) any(s[[k]]))))
})

test_that("a stratum without rows is not reported as available", {
  live <- dplyr::mutate(sample_terminal(), is_live = TRUE)
  s <- sample_gold_check_rows(sample_panels(), live)
  expect_false("terminal_month" %in% attr(s, "available_strata"))
})

test_that("the draw is reproducible and leaves the global seed alone", {
  set.seed(1)
  before <- stats::runif(1)
  set.seed(1)
  a <- sample_gold_check_rows(sample_panels(), sample_terminal())
  after <- stats::runif(1)
  b <- sample_gold_check_rows(sample_panels(), sample_terminal())
  expect_identical(a, b)
  expect_equal(after, before)
  expect_error(
    sample_gold_check_rows(sample_panels(), sample_terminal(), n_per_stratum = 0), "at least 1"
  )
})
