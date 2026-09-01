test_that("input is standardized, scaled, sorted, and tagged with a time zone", {
  # Create input with timestamps intentionally arranged in reverse order.
  input <- data.frame(
    datetime = c("2026-01-01 00:00:01", "2026-01-01 00:00:00"),
    V1 = c(100, 99),
    V2 = c(10, 9)
  )

  # Standardize column names, scale spot prices, parse timestamps,
  # and sort observations chronologically.
  result <- validate_and_standardize_input(
    data = input,
    datetime_col = "datetime",
    futures_col = "V1",
    spot_col = "V2",
    spot_multiplier = 10,
    tz = "America/New_York"
  )

  # The standardized output should contain exactly four documented columns.
  expect_named(result, c("datetime", "trading_day", "futures", "spot"))

  # The timestamp and trading-day columns should have the expected classes.
  expect_s3_class(result$datetime, "POSIXct")
  expect_s3_class(result$trading_day, "Date")

  # The observations should be sorted in chronological order.
  expect_equal(
    format(result$datetime, tz = "America/New_York", usetz = FALSE),
    c("2026-01-01 00:00:00", "2026-01-01 00:00:01")
  )

  # Both observations should be assigned to the same New York trading day.
  expect_equal(result$trading_day, as.Date(c("2026-01-01", "2026-01-01")))

  # Futures prices should follow the chronological timestamp order.
  expect_equal(result$futures, c(99, 100))

  # Spot prices should be multiplied by ten and then chronologically sorted.
  expect_equal(result$spot, c(90, 100))

  # The selected time zone should be stored as a data-frame attribute.
  expect_identical(attr(result, "tz"), "America/New_York")

  # Row names should be reset after chronological sorting.
  expect_identical(rownames(result), c("1", "2"))
})

test_that("required input columns must be present", {
  # Create input without the requested V2 spot-price column.
  input <- data.frame(
    datetime = "2026-01-01 00:00:00",
    V1 = 100
  )

  # The function should identify the exact missing column.
  expect_error(
    validate_and_standardize_input(
      data = input,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      tz = "America/New_York"
    ),
    "Missing required columns: V2",
    fixed = TRUE
  )
})

test_that("price columns must be numeric", {
  # V1 looks like a number but is stored as a character string.
  input <- data.frame(
    datetime = "2026-01-01 00:00:00",
    V1 = "100",
    V2 = 10
  )

  # Character price columns should not be silently converted to numeric values.
  expect_error(
    validate_and_standardize_input(
      data = input,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      tz = "America/New_York"
    ),
    "Both price columns must be numeric.",
    fixed = TRUE
  )
})

test_that("non-positive and non-finite observed prices are rejected", {
  # A zero futures price violates the strictly positive price requirement.
  zero_price <- data.frame(
    datetime = "2026-01-01 00:00:00",
    V1 = 0,
    V2 = 10
  )

  # An infinite spot price violates the finite price requirement.
  infinite_price <- data.frame(
    datetime = "2026-01-01 00:00:00",
    V1 = 100,
    V2 = Inf
  )

  # Zero prices should be rejected before any later calculation.
  expect_error(
    validate_and_standardize_input(
      data = zero_price,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      tz = "America/New_York"
    ),
    "strictly positive"
  )

  # Infinite prices should be rejected by the same price-quality check.
  expect_error(
    validate_and_standardize_input(
      data = infinite_price,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      tz = "America/New_York"
    ),
    "strictly positive"
  )
})

test_that("a locally missing price is allowed for an inactive market", {
  # The spot price is missing in the first row but observed in the second row.
  input <- data.frame(
    datetime = c("2026-01-01 00:00:00", "2026-01-01 00:00:01"),
    V1 = c(100, 101),
    V2 = c(NA_real_, 10)
  )

  # Standardize the input and apply the spot-price multiplier.
  result <- validate_and_standardize_input(
    data = input,
    datetime_col = "datetime",
    futures_col = "V1",
    spot_col = "V2",
    spot_multiplier = 10,
    tz = "America/New_York"
  )

  # A locally missing price should remain missing rather than causing an error.
  expect_true(is.na(result$spot[[1L]]))

  # The observed spot price should still be multiplied by ten.
  expect_equal(result$spot[[2L]], 100)
})

test_that("an entirely missing price series is rejected", {
  # Every spot observation is missing, so the series cannot be analyzed.
  input <- data.frame(
    datetime = c("2026-01-01 00:00:00", "2026-01-01 00:00:01"),
    V1 = c(100, 101),
    V2 = c(NA_real_, NA_real_)
  )

  # A completely missing market series should stop the standardization.
  expect_error(
    validate_and_standardize_input(
      data = input,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      tz = "America/New_York"
    ),
    "Neither price series may be entirely missing.",
    fixed = TRUE
  )
})

test_that("missing timestamps are dropped by default", {
  # Use an explicit missing timestamp to test the default removal behavior.
  input <- data.frame(
    datetime = c("2026-01-01 00:00:00", NA_character_),
    V1 = c(100, 101),
    V2 = c(10, 11)
  )

  # drop_missing_datetime defaults to TRUE.
  result <- validate_and_standardize_input(
    data = input,
    datetime_col = "datetime",
    futures_col = "V1",
    spot_col = "V2",
    tz = "America/New_York"
  )

  # The row containing the missing timestamp should be removed.
  expect_equal(nrow(result), 1L)

  # The remaining futures price should come from the valid row.
  expect_equal(result$futures, 100)

  # The remaining spot price should be multiplied by the default factor of ten.
  expect_equal(result$spot, 100)
})

test_that("missing timestamps can be treated as an error", {
  # Use one valid timestamp and one explicitly missing timestamp.
  input <- data.frame(
    datetime = c("2026-01-01 00:00:00", NA_character_),
    V1 = c(100, 101),
    V2 = c(10, 11)
  )

  # Disabling timestamp removal should convert missing timestamps into an error.
  expect_error(
    validate_and_standardize_input(
      data = input,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      tz = "America/New_York",
      drop_missing_datetime = FALSE
    ),
    "missing or unparseable"
  )
})

test_that("duplicate timestamps are rejected", {
  # Create two observations with exactly the same timestamp.
  input <- data.frame(
    datetime = c("2026-01-01 00:00:00", "2026-01-01 00:00:00"),
    V1 = c(100, 101),
    V2 = c(10, 11)
  )

  # Duplicate timestamps would make one-second observations ambiguous.
  expect_error(
    validate_and_standardize_input(
      data = input,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      tz = "America/New_York"
    ),
    "duplicate observations"
  )
})

test_that("spot multiplier and time zone must be valid scalar inputs", {
  # Create a valid data frame so that only parameter validation is tested.
  input <- data.frame(
    datetime = "2026-01-01 00:00:00",
    V1 = 100,
    V2 = 10
  )

  # An infinite spot multiplier is not a valid finite scalar.
  expect_error(
    validate_and_standardize_input(
      data = input,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      spot_multiplier = Inf,
      tz = "America/New_York"
    ),
    "one finite number"
  )

  # An empty string does not identify a usable time zone.
  expect_error(
    validate_and_standardize_input(
      data = input,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      tz = ""
    ),
    "one non-empty time-zone string"
  )
})
