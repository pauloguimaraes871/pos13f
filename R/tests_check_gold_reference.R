#' Gold suite block: sampled panel rows agree with a dplyr recomputation
#'
#' Called by [run_test_gold()]. For the rows drawn by `sample_gold_check_rows()`,
#' compares breadth, F1, the identity count, liquidity, volatility, signal controls and
#' forward returns with `build_gold_reference()`, which recomputes them from silver in
#' dplyr without the SQL. The sampled ids are printed, so the log says what was checked.
#'
#' @param panels Named list from `build_gold_panels()`.
#' @param cohort Tibble from `select_gold_cohort()`.
#' @param reference Named list from `build_gold_reference()`.
#' @param tolerance Numeric scalar, relative tolerance of `testthat::expect_equal()`
#'   (floating-point sums in a different order differ in the last digits).
#'
#' @return `NULL`, invisibly; called for its `testthat` expectations.
#' @noRd
check_gold_reference <- function(panels, cohort, reference, tolerance = 1e-8) {
  sample <- reference$sample
  message(
    "Recomputed ", nrow(sample), " panel rows (",
    paste(names(table(sample$stratum)), table(sample$stratum), sep = ": ", collapse = ", "),
    "): ", paste(sample$id, collapse = ", ")
  )
  pick <- function(m_df) {
    dplyr::semi_join(m_df, sample, by = dplyr::join_by("id")) |> dplyr::arrange(.data$id)
  }
  ordered <- dplyr::arrange(sample, .data$id)

  testthat::test_that("the sample is not empty and covers every stratum that had rows", {
    testthat::expect_gt(nrow(sample), 0)
    testthat::expect_gt(length(reference$available_strata), 0)
    for (s in reference$available_strata) {
      testthat::expect_true(any(sample[[s]]), label = paste("stratum", s))
    }
    testthat::expect_gt(nrow(reference$activity), 0)
  })

  testthat::test_that("sampled cohort activity counts match the dplyr recomputation", {
    compared <- reference$activity |>
      dplyr::inner_join(cohort,
        by = dplyr::join_by("formation_date", "cik"), relationship = "one-to-one",
        unmatched = c("error", "drop"), suffix = c("_ref", "")
      )
    for (col in c("n_prev", "n_curr", "n_both")) {
      testthat::expect_equal(compared[[col]], compared[[paste0(col, "_ref")]], label = col)
    }
    testthat::expect_equal(compared$value_curr, compared$value_curr_ref, tolerance = tolerance)
  })

  testthat::test_that("sampled breadth and F1 match the dplyr recomputation", {
    sizes <- dplyr::summarise(cohort, cohort_size = sum(.data$in_cohort), .by = "formation_date")
    expected <- ordered |>
      dplyr::left_join(dplyr::rename(reference$breadth, tickers = "instrument_id"),
        by = dplyr::join_by("formation_date", "tickers"), relationship = "many-to-one"
      ) |>
      dplyr::left_join(sizes, by = dplyr::join_by("formation_date"), relationship = "many-to-one")
    actual <- pick(panels$signals_m_df)
    testthat::expect_equal(actual$n_holders_prev, as.numeric(expected$n_prev))
    testthat::expect_equal(actual$n_holders_curr, as.numeric(expected$n_curr))
    testthat::expect_equal(actual$n_initiations, as.numeric(expected$n_init))
    testthat::expect_equal(actual$n_exits, as.numeric(expected$n_exit))
    testthat::expect_equal(actual$f1_breadth_change,
      (expected$n_curr - expected$n_prev) / expected$cohort_size,
      tolerance = tolerance
    )
    diagnostics <- panels$universe_diagnostics |>
      dplyr::select("formation_date", tickers = "instrument_id", "n_match")
    matched <- dplyr::left_join(expected, diagnostics,
      by = dplyr::join_by("formation_date", "tickers"), relationship = "many-to-one"
    )
    testthat::expect_equal(as.numeric(matched$n_match.y), as.numeric(matched$n_match.x))
  })

  testthat::test_that("sampled features match the dplyr recomputation", {
    as_of <- function(features, date_col) {
      ordered |>
        dplyr::select("id", "tickers", as_of = dplyr::all_of(date_col)) |>
        dplyr::left_join(dplyr::rename(features, tickers = "instrument_id"),
          by = dplyr::join_by("tickers", "as_of"), relationship = "many-to-one"
        )
    }
    row <- as_of(reference$row_features, "dates")
    liquidity <- pick(panels$liquidity_m_df)
    volatility <- pick(panels$volatility_m_df)
    for (col in c("mean_volfin_3m", "median_volfin_3m", "presence", "close_prev")) {
      testthat::expect_equal(liquidity[[col]], row[[col]], tolerance = tolerance, label = col)
    }
    ## The panel falls back to vol_3m when daily_vol is undefined (factoRverse needs a value)
    testthat::expect_equal(volatility$daily_vol, dplyr::coalesce(row$daily_vol, row$vol_3m),
      tolerance = tolerance
    )
    testthat::expect_equal(volatility$vol_3m, row$vol_3m, tolerance = tolerance)
    signal <- as_of(reference$signal_features, "label_date")
    signals <- pick(panels$signals_m_df)
    for (col in c("mom_12_1", "rev_1m", "vol_3m", "amihud")) {
      testthat::expect_equal(signals[[col]], signal[[col]], tolerance = tolerance, label = col)
    }
  })

  testthat::test_that("sampled forward returns match the daily compounding", {
    expected <- ordered |>
      dplyr::left_join(reference$returns,
        by = dplyr::join_by("tickers", "dates"), relationship = "one-to-one"
      )
    actual <- pick(panels$fwd_1m_returns_m_df)
    testthat::expect_equal(actual$fwd_return_1m, expected$fwd_return_1m, tolerance = tolerance)
    testthat::expect_equal(actual$fwd_return_1m_stress, expected$fwd_return_1m_stress,
      tolerance = tolerance
    )
  })
  invisible(NULL)
}
