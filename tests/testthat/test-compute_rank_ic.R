## Two formations of four stocks; the signal orders the 3-month returns exactly
ic_fixture <- function() {
  signals <- tibble::tibble(
    tickers = rep(paste0("S", 1:4), 6),
    dates = rep(seq(as.Date("2020-03-01"), by = "month", length.out = 6), each = 4),
    signal_age_months = rep(c(0, 1, 2, 0, 1, 2), each = 4),
    f1 = rep(c(0.1, 0.2, 0.3, 0.4), 6)
  ) |>
    dplyr::mutate(id = paste0(.data$tickers, "-", .data$dates))
  fwd <- dplyr::transmute(signals, id = .data$id, fwd_return_1m = rep(c(-2, 0, 1, 3), 6))
  list(signals = signals, fwd = fwd)
}

test_that("a signal that orders returns perfectly has an IC of 1 in every formation", {
  x <- ic_fixture()
  out <- compute_rank_ic(x$signals, x$fwd, "f1")
  expect_equal(out$by_formation$ic, c(1, 1))
  expect_equal(out$summary$n_formations, 2)
})

test_that("a formation with an unfinished month is left out", {
  x <- ic_fixture()
  last <- x$signals$id[x$signals$dates == max(x$signals$dates)]
  x$fwd <- dplyr::mutate(x$fwd,
    fwd_return_1m = dplyr::if_else(.data$id %in% last, NA, .data$fwd_return_1m)
  )
  out <- compute_rank_ic(x$signals, x$fwd, "f1")
  expect_equal(out$by_formation$formation, as.Date("2020-03-01"))
})
