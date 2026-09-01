# Create a minimal successful daily CWIS result for aggregation tests.
.make_aggregate_test_day <- function(
    date,
    his_midpoint,
    cwis,
    overlapping_weight,
    residual_correlation,
    zero_return_fraction,
    variance_weights,
    n_observations = 86400L,
    n_overlap = 54000L) {
  structure(
    list(
      date = as.Date(date),
      his_midpoint = his_midpoint,
      cwis = cwis,
      overlapping_weight = overlapping_weight,
      residual_correlation = residual_correlation,
      zero_return_fraction = zero_return_fraction,
      variance_weights = variance_weights,
      n_observations = n_observations,
      n_overlap = n_overlap
    ),
    class = "cwis_day_result"
  )
}


# Create two successful days and one failed day.
.make_mixed_aggregate_results <- function() {
  first_day <- .make_aggregate_test_day(
    date = "2026-01-04",
    his_midpoint = c(
      futures = 0.40,
      spot = 0.60
    ),
    cwis = c(
      futures = 0.43,
      spot = 0.57
    ),
    overlapping_weight = 0.70,
    residual_correlation = 0.80,
    zero_return_fraction = c(
      futures = 0.10,
      spot = 0.20
    ),
    variance_weights = c(
      futures_pre = 0.10,
      spot_maintenance = 0.15,
      futures_post = 0.05,
      overlap_pre = 0.20,
      overlap_core = 0.30,
      overlap_post = 0.20
    )
  )

  second_day <- .make_aggregate_test_day(
    date = "2026-01-05",
    his_midpoint = c(
      futures = 0.60,
      spot = 0.40
    ),
    cwis = c(
      futures = 0.55,
      spot = 0.45
    ),
    overlapping_weight = 0.50,
    residual_correlation = 0.60,
    zero_return_fraction = c(
      futures = 0.30,
      spot = 0.40
    ),
    variance_weights = c(
      futures_pre = 0.15,
      spot_maintenance = 0.25,
      futures_post = 0.10,
      overlap_pre = 0.10,
      overlap_core = 0.20,
      overlap_post = 0.20
    ),
    n_observations = 85000L,
    n_overlap = 52000L
  )

  failed_day <- list(
    date = as.Date("2026-01-06"),
    error = "Synthetic daily calculation failure."
  )

  list(
    first_day,
    second_day,
    failed_day
  )
}


test_that("aggregate_cwis creates the documented output structure", {
  results <- .make_mixed_aggregate_results()

  aggregated <- aggregate_cwis(results)

  # The function should return four documented components.
  expect_type(aggregated, "list")
  expect_named(
    aggregated,
    c(
      "daily",
      "information_share",
      "diagnostics",
      "variance_weights"
    )
  )

  # Each component should be a data frame.
  expect_s3_class(aggregated$daily, "data.frame")
  expect_s3_class(
    aggregated$information_share,
    "data.frame"
  )
  expect_s3_class(
    aggregated$diagnostics,
    "data.frame"
  )
  expect_s3_class(
    aggregated$variance_weights,
    "data.frame"
  )
})


test_that("successful and failed days are recorded in one daily table", {
  results <- .make_mixed_aggregate_results()

  aggregated <- aggregate_cwis(results)
  daily <- aggregated$daily

  # One row should be produced for each supplied day.
  expect_equal(nrow(daily), 3L)

  # The dates should preserve the order of the input list.
  expect_equal(
    daily$date,
    as.Date(
      c(
        "2026-01-04",
        "2026-01-05",
        "2026-01-06"
      )
    )
  )

  # Successful and failed days should receive different status values.
  expect_equal(
    daily$status,
    c("ok", "ok", "failed")
  )

  # Successful days should not contain an error message.
  expect_true(
    all(is.na(daily$error[1:2]))
  )

  # The failed day should preserve its original error message.
  expect_identical(
    daily$error[[3L]],
    "Synthetic daily calculation failure."
  )

  # Successful daily statistics should be copied into the daily table.
  expect_equal(
    daily$his_futures[1:2],
    c(0.40, 0.60)
  )
  expect_equal(
    daily$his_spot[1:2],
    c(0.60, 0.40)
  )
  expect_equal(
    daily$cwis_futures[1:2],
    c(0.43, 0.55)
  )
  expect_equal(
    daily$cwis_spot[1:2],
    c(0.57, 0.45)
  )

  # Failed-day numerical results should be stored as missing values.
  failed_numeric_columns <- c(
    "his_futures",
    "his_spot",
    "cwis_futures",
    "cwis_spot",
    "overlapping_weight",
    "residual_correlation",
    "zero_return_futures",
    "zero_return_spot",
    "n_observations",
    "n_overlap"
  )

  expect_true(
    all(vapply(
      daily[3L, failed_numeric_columns, drop = FALSE],
      function(x) is.na(x[[1L]]),
      logical(1L)
    ))
  )
})


test_that("information-share means and standard deviations use successful days only", {
  results <- .make_mixed_aggregate_results()

  aggregated <- aggregate_cwis(results)
  summary <- aggregated$information_share

  # The summary should contain the four documented statistics.
  expect_equal(
    summary$statistic,
    c(
      "HIS mean",
      "HIS SD",
      "CWIS mean",
      "CWIS SD"
    )
  )

  expected_futures <- c(
    mean(c(0.40, 0.60)),
    stats::sd(c(0.40, 0.60)),
    mean(c(0.43, 0.55)),
    stats::sd(c(0.43, 0.55))
  )

  expected_spot <- c(
    mean(c(0.60, 0.40)),
    stats::sd(c(0.60, 0.40)),
    mean(c(0.57, 0.45)),
    stats::sd(c(0.57, 0.45))
  )

  # The failed day should not enter the mean or standard deviation.
  expect_equal(
    summary$futures,
    expected_futures,
    tolerance = 1e-12
  )
  expect_equal(
    summary$spot,
    expected_spot,
    tolerance = 1e-12
  )
})


test_that("variance weights are summarized by period", {
  results <- .make_mixed_aggregate_results()

  aggregated <- aggregate_cwis(results)
  summary <- aggregated$variance_weights

  first_weights <- results[[1L]]$variance_weights
  second_weights <- results[[2L]]$variance_weights

  weights_matrix <- rbind(
    first_weights,
    second_weights
  )

  # The output should preserve all variance-weight period names.
  expect_equal(
    summary$period,
    colnames(weights_matrix)
  )

  # Period means should be calculated across successful days.
  expect_equal(
    summary$mean,
    unname(
    apply(
      weights_matrix,
      2L,
      mean
     )
    ),
    tolerance = 1e-12
  )


  # Period standard deviations should use the successful daily weights.
  expect_equal(
    summary$sd,
    unname(
    apply(
      weights_matrix,
      2L,
      stats::sd
     )
    ),
    tolerance = 1e-12
  )
})


test_that("diagnostics count days and summarize successful observations", {
  results <- .make_mixed_aggregate_results()

  aggregated <- aggregate_cwis(results)
  diagnostics <- aggregated$diagnostics

  # Create a named vector for convenient diagnostic lookup.
  diagnostic_values <- stats::setNames(
    diagnostics$value,
    diagnostics$statistic
  )

  # The input contains two successful days and one failed day.
  expect_equal(
    diagnostic_values[["days supplied"]],
    3
  )
  expect_equal(
    diagnostic_values[["days successful"]],
    2
  )
  expect_equal(
    diagnostic_values[["days failed"]],
    1
  )

  # Failed-day missing values should be ignored in diagnostic means.
  expect_equal(
    diagnostic_values[["mean overlapping weight"]],
    mean(c(0.70, 0.50)),
    tolerance = 1e-12
  )
  expect_equal(
    diagnostic_values[["SD overlapping weight"]],
    stats::sd(c(0.70, 0.50)),
    tolerance = 1e-12
  )
  expect_equal(
    diagnostic_values[["mean residual correlation"]],
    mean(c(0.80, 0.60)),
    tolerance = 1e-12
  )
})


test_that("one successful day produces means but undefined standard deviations", {
  first_day <- .make_mixed_aggregate_results()[[1L]]

  aggregated <- aggregate_cwis(
    list(first_day)
  )

  summary <- aggregated$information_share

  # Means are defined when one successful day is available.
  expect_equal(
    summary$futures[
      summary$statistic == "HIS mean"
    ],
    0.40
  )
  expect_equal(
    summary$spot[
      summary$statistic == "HIS mean"
    ],
    0.60
  )
  expect_equal(
    summary$futures[
      summary$statistic == "CWIS mean"
    ],
    0.43
  )
  expect_equal(
    summary$spot[
      summary$statistic == "CWIS mean"
    ],
    0.57
  )

  # Sample standard deviations require at least two observations.
  expect_true(
    is.na(
      summary$futures[
        summary$statistic == "HIS SD"
      ]
    )
  )
  expect_true(
    is.na(
      summary$spot[
        summary$statistic == "HIS SD"
      ]
    )
  )
  expect_true(
    is.na(
      summary$futures[
        summary$statistic == "CWIS SD"
      ]
    )
  )
  expect_true(
    is.na(
      summary$spot[
        summary$statistic == "CWIS SD"
      ]
    )
  )

  # Each variance-weight standard deviation should also be missing.
  expect_true(
    all(is.na(aggregated$variance_weights$sd))
  )

  diagnostic_values <- stats::setNames(
    aggregated$diagnostics$value,
    aggregated$diagnostics$statistic
  )

  # The overlap-weight SD is undefined for one successful day.
  expect_true(
    is.na(
      diagnostic_values[["SD overlapping weight"]]
    )
  )
})


test_that("all failed days produce NA summaries without stopping aggregation", {
  failed_results <- list(
    list(
      date = as.Date("2026-01-04"),
      error = "First synthetic failure."
    ),
    list(
      date = as.Date("2026-01-05"),
      error = "Second synthetic failure."
    )
  )

  aggregated <- aggregate_cwis(
    failed_results
  )

  # Both days should remain visible in the daily table.
  expect_equal(
    aggregated$daily$status,
    c("failed", "failed")
  )
  expect_equal(
    aggregated$daily$error,
    c(
      "First synthetic failure.",
      "Second synthetic failure."
    )
  )

  # No successful values are available for HIS or CWIS summaries.
  expect_true(
    all(is.na(aggregated$information_share$futures))
  )
  expect_true(
    all(is.na(aggregated$information_share$spot))
  )

  # No successful variance-weight vectors are available.
  expect_equal(
    nrow(aggregated$variance_weights),
    0L
  )

  diagnostic_values <- stats::setNames(
    aggregated$diagnostics$value,
    aggregated$diagnostics$statistic
  )

  expect_equal(
    diagnostic_values[["days supplied"]],
    2
  )
  expect_equal(
    diagnostic_values[["days successful"]],
    0
  )
  expect_equal(
    diagnostic_values[["days failed"]],
    2
  )

  # Aggregate diagnostics based on successful values should be missing.
  expect_true(
    is.na(
      diagnostic_values[["mean overlapping weight"]]
    )
  )
  expect_true(
    is.na(
      diagnostic_values[["SD overlapping weight"]]
    )
  )
  expect_true(
    is.na(
      diagnostic_values[["mean residual correlation"]]
    )
  )
})


test_that("an empty result list produces stable empty summaries", {
  aggregated <- aggregate_cwis(
    list()
  )

  # No daily rows should be created.
  expect_equal(
    nrow(aggregated$daily),
    0L
  )

  # The standard information-share table should still be returned.
  expect_equal(
    aggregated$information_share$statistic,
    c(
      "HIS mean",
      "HIS SD",
      "CWIS mean",
      "CWIS SD"
    )
  )
  expect_true(
    all(is.na(aggregated$information_share$futures))
  )
  expect_true(
    all(is.na(aggregated$information_share$spot))
  )

  # No variance-weight summary can be calculated.
  expect_equal(
    nrow(aggregated$variance_weights),
    0L
  )

  diagnostic_values <- stats::setNames(
    aggregated$diagnostics$value,
    aggregated$diagnostics$statistic
  )

  expect_equal(
    diagnostic_values[["days supplied"]],
    0
  )
  expect_equal(
    diagnostic_values[["days successful"]],
    0
  )
  expect_equal(
    diagnostic_values[["days failed"]],
    0
  )

  expect_true(
    is.na(
      diagnostic_values[["mean overlapping weight"]]
    )
  )
})


test_that("results must be supplied as a list", {
  expect_error(
    aggregate_cwis(
      c(1, 2, 3)
    ),
    "`results` must be a list.",
    fixed = TRUE
  )
})
