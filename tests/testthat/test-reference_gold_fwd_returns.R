## FIGI_A trades through October; FIGI_B stops on 2019-10-15 and never trades again
fwd_returns_d <- function() {
  tibble::tibble(
    instrument_id = c("FIGI_A", "FIGI_A", "FIGI_A", "FIGI_B", "FIGI_B"),
    date = as.Date(c("2019-09-30", "2019-10-01", "2019-10-31", "2019-09-30", "2019-10-15")),
    ret = c(0.10, 0.02, -0.01, 0.05, NA)
  )
}
fwd_terminal <- function() {
  tibble::tibble(
    instrument_id = c("FIGI_A", "FIGI_B"), last_date = as.Date(c("2019-11-15", "2019-10-15")),
    is_live = c(TRUE, FALSE)
  )
}

test_that("a month's return compounds its own daily returns, with the terminal policy", {
  rows <- tibble::tibble(
    tickers = rep(c("FIGI_A", "FIGI_B"), each = 3),
    dates = rep(as.Date(c("2019-09-01", "2019-10-01", "2019-11-01")), 2)
  )
  out <- reference_gold_fwd_returns(rows, fwd_returns_d(), fwd_terminal(),
    last_month = as.Date("2019-11-01")
  )
  a <- dplyr::filter(out, .data$tickers == "FIGI_A")
  b <- dplyr::filter(out, .data$tickers == "FIGI_B")
  expect_equal(a$fwd_return_1m[1:2], c(10, 100 * (1.02 * 0.99 - 1)))
  expect_equal(a$fwd_return_1m_stress[1:2], a$fwd_return_1m[1:2])
  ## B's last price month: a missing return earns nothing, the stress applies -30%
  expect_equal(b$fwd_return_1m[2], 0)
  expect_equal(b$fwd_return_1m_stress[2], -30)
  ## November is the unfinished month
  expect_true(all(is.na(out$fwd_return_1m[out$dates == as.Date("2019-11-01")])))
})

test_that("the reference reproduces the panel's forward returns on the real-data toy", {
  toy <- toy_silver_gold()
  con <- toy_gold_con(toy)
  monthly <- toy_gold_sql(con, sql_gold_monthly_returns("silver_returns_d"))
  ## Real, unbalanced rows: every month in which an instrument has a price, plus the month
  ## after its last price (where the closed position earns 0)
  priced <- toy$prices_d |>
    dplyr::filter(.data$date >= as.Date("2019-12-01")) |>
    dplyr::distinct(
      tickers = .data$instrument_id, dates = lubridate::floor_date(.data$date, "month")
    )
  after_last <- dplyr::transmute(
    dplyr::slice_max(priced, .data$dates, by = "tickers"),
    .data$tickers,
    dates = add_months(.data$dates, 1)
  )
  rows <- dplyr::bind_rows(priced, after_last) |>
    dplyr::filter(.data$dates <= toy_gold_last_month) |>
    dplyr::distinct() |>
    dplyr::mutate(id = paste0(.data$tickers, "-", .data$dates)) |>
    dplyr::arrange(.data$id)
  panel <- gold_return_columns(rows, c("id", "tickers", "dates"), monthly, toy$terminal,
    toy_gold_last_month,
    terminal_stress = -0.3
  )
  ref <- reference_gold_fwd_returns(rows, toy$returns_d, toy$terminal, toy_gold_last_month) |>
    dplyr::mutate(id = paste0(.data$tickers, "-", .data$dates)) |>
    dplyr::arrange(.data$id)
  expect_equal(ref$fwd_return_1m, panel$fwd_return_1m, tolerance = 1e-10)
  expect_equal(ref$fwd_return_1m_stress, panel$fwd_return_1m_stress, tolerance = 1e-10)
  expect_true(any(ref$fwd_return_1m_stress < ref$fwd_return_1m, na.rm = TRUE))
})
