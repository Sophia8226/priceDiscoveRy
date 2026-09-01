# Load and standardize one complete trading day from the package data.
.load_daily_cwis_test_data <- function(
    test_date = "2019-06-24") {
  data_name <-
    "example_sp500_futures_spot_oneweek_24h_1sec"

  data_environment <- new.env(
    parent = baseenv()
  )

  # Load package data into an isolated environment.
  utils::data(
    list = data_name,
    package = "priceDiscoveRy",
    envir = data_environment
  )

  raw_data <- get(
    data_name,
    envir = data_environment
  )

  # Convert the bundled data to a regular data frame.
  raw_data <- as.data.frame(raw_data)

  # Select one complete weekday.
  raw_day <- raw_data[
    raw_data$date == test_date,
    ,
    drop = FALSE
  ]

  if (nrow(raw_day) == 0L) {
    stop(
      "The requested test date is not available in the example data.",
      call. = FALSE
    )
  }

  # Produce the standardized input required by daily_cwis().
  validate_and_standardize_input(
    data = raw_day,
    datetime_col = "datetime",
    futures_col = "V1",
    spot_col = "V2",
    spot_multiplier = 10,
    tz = "America/New_York"
  )
}


test_that("slice_returns divides returns according to period lengths", {
  returns <- seq_len(9L)

  lengths <- c(
    overlap_pre = 2L,
    overlap_core = 3L,
    overlap_post = 4L
  )

  parts <- priceDiscoveRy:::.slice_returns(
    returns = returns,
    lengths = lengths
  )

  # The parts should preserve the period names.
  expect_named(
    parts,
    c(
      "overlap_pre",
      "overlap_core",
      "overlap_post"
    )
  )

  # Each period should receive its exact sequential return range.
  expect_equal(parts$overlap_pre, 1:2)
  expect_equal(parts$overlap_core, 3:5)
  expect_equal(parts$overlap_post, 6:9)

  # No return should be lost or duplicated.
  expect_equal(
    unname(unlist(parts)),
    returns
  )
})


test_that("daily_cwis rejects insufficient trading sessions", {
  input <- data.frame(
    datetime = as.POSIXct(
      c(
        "2026-01-04 04:00:00",
        "2026-01-04 04:00:01"
      ),
      tz = "America/New_York"
    ),
    trading_day = as.Date("2026-01-04"),
    futures = c(3700, 3701),
    spot = c(3700, 3701)
  )

  attr(input, "tz") <- "America/New_York"

  # Only overlap_pre contains enough observations.
  expect_error(
    daily_cwis(
      day_data = input,
      K = 2L
    ),
    "Insufficient observations in sessions: futures_pre",
    fixed = TRUE
  )
})


test_that("daily_cwis produces internally consistent results", {
  day_data <- .load_daily_cwis_test_data(
    test_date = "2019-06-24"
  )

  # Run the complete daily workflow only once.
  result <- daily_cwis(
    day_data = day_data,
    K = 10L,
    kernel_type = "ModifiedTukeyHanning",
    bandwidth_constant = NULL,
    align_by = "seconds",
    align_period = 1L
  )

  # Check the output class and documented components.
  expect_s3_class(
    result,
    "cwis_day_result"
  )
  expect_named(
    result,
    c(
      "date",
      "his_lower",
      "his_upper",
      "his_midpoint",
      "residual_correlation",
      "zero_return_fraction",
      "realized_variance",
      "variance_weights",
      "overlapping_weight",
      "cwis",
      "n_observations",
      "n_overlap"
    )
  )

  # Check the date and observation counts.
  expect_equal(
    result$date,
    as.Date("2019-06-24")
  )
  expect_equal(
    result$n_observations,
    nrow(day_data)
  )
  expect_true(result$n_overlap > 0L)
  expect_true(
    result$n_overlap <= result$n_observations
  )

  # Check HIS names and normalization.
  expect_named(
    result$his_lower,
    c("futures", "spot")
  )
  expect_named(
    result$his_upper,
    c("futures", "spot")
  )
  expect_named(
    result$his_midpoint,
    c("futures", "spot")
  )
  expect_true(all(is.finite(result$his_lower)))
  expect_true(all(is.finite(result$his_upper)))
  expect_true(all(is.finite(result$his_midpoint)))

  expect_equal(
    sum(result$his_lower),
    1,
    tolerance = 1e-8
  )
  expect_equal(
    sum(result$his_upper),
    1,
    tolerance = 1e-8
  )
  expect_equal(
    result$his_midpoint,
    (result$his_lower + result$his_upper) / 2,
    tolerance = 1e-10
  )

  # Check period variance and weight names.
  expected_periods <- c(
    "futures_pre",
    "spot_maintenance",
    "futures_post",
    "overlap_pre",
    "overlap_core",
    "overlap_post"
  )

  expect_named(
    result$realized_variance,
    expected_periods
  )
  expect_named(
    result$variance_weights,
    expected_periods
  )

  # Check variances and normalized weights.
  expect_true(
    all(is.finite(result$realized_variance))
  )
  expect_true(
    all(result$realized_variance >= 0)
  )
  expect_true(
    all(is.finite(result$variance_weights))
  )
  expect_true(
    all(result$variance_weights >= 0)
  )
  expect_equal(
    sum(result$variance_weights),
    1,
    tolerance = 1e-8
  )

  expected_overlap_weight <- sum(
    result$variance_weights[
      c(
        "overlap_pre",
        "overlap_core",
        "overlap_post"
      )
    ]
  )

  expect_equal(
    result$overlapping_weight,
    expected_overlap_weight,
    tolerance = 1e-10
  )

  # Reproduce the final daily CWIS formula.
  weights <- result$variance_weights

  expected_cwis <-
    result$overlapping_weight *
    result$his_midpoint +
    c(
      futures =
        weights[["futures_pre"]] +
        weights[["futures_post"]],
      spot =
        weights[["spot_maintenance"]]
    )

  expect_equal(
    result$cwis,
    expected_cwis,
    tolerance = 1e-10
  )

  # The two market contributions should sum to one.
  expect_named(
    result$cwis,
    c("futures", "spot")
  )
  expect_true(all(is.finite(result$cwis)))
  expect_true(all(result$cwis >= 0))
  expect_true(all(result$cwis <= 1))
  expect_equal(
    sum(result$cwis),
    1,
    tolerance = 1e-8
  )

  # Check diagnostic ranges.
  expect_true(
    is.finite(result$residual_correlation)
  )
  expect_true(
    result$residual_correlation >= -1
  )
  expect_true(
    result$residual_correlation <= 1
  )

  expect_named(
    result$zero_return_fraction,
    c("futures", "spot")
  )
  expect_true(
    all(is.finite(result$zero_return_fraction))
  )
  expect_true(
    all(result$zero_return_fraction >= 0)
  )
  expect_true(
    all(result$zero_return_fraction <= 1)
  )
})
