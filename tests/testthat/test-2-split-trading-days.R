test_that("standardized data are split into named trading days", {
  input <- data.frame(
    datetime = as.POSIXct(
      c(
        "2026-01-01 09:30:00",
        "2026-01-01 09:30:01",
        "2026-01-02 09:30:00"
      ),
      tz = "America/New_York"
    ),
    trading_day = as.Date(
      c(
        "2026-01-01",
        "2026-01-01",
        "2026-01-02"
      )
    ),
    futures = c(100, 101, 102),
    spot = c(200, 201, 202)
  )

  # Use non-consecutive original row names to verify that row names are reset
  # after the data are split.
  rownames(input) <- c("5", "6", "10")

  # Reproduce the time-zone attribute stored by
  # validate_and_standardize_input().
  attr(input, "tz") <- "America/New_York"

  result <- split_trading_days(input)

  # Verify that the result is a list containing two trading days.
  expect_type(result, "list")
  expect_length(result, 2L)

  # Verify that the list is named according to the trading dates.
  expect_named(
    result,
    c("2026-01-01", "2026-01-02")
  )

  # Verify the number of observations for each trading day.
  expect_equal(
    nrow(result[["2026-01-01"]]),
    2L
  )
  expect_equal(
    nrow(result[["2026-01-02"]]),
    1L
  )

  # Verify that observations are assigned to the correct trading day.
  expect_equal(
    result[["2026-01-01"]]$futures,
    c(100, 101)
  )
  expect_equal(
    result[["2026-01-02"]]$futures,
    102
  )

  expect_equal(
    result[["2026-01-01"]]$spot,
    c(200, 201)
  )
  expect_equal(
    result[["2026-01-02"]]$spot,
    202
  )

  # Verify that consecutive row names are assigned within each daily data set.
  expect_identical(
    rownames(result[["2026-01-01"]]),
    c("1", "2")
  )
  expect_identical(
    rownames(result[["2026-01-02"]]),
    "1"
  )

  # Verify that the time-zone attribute is passed to each daily data set.
  expect_identical(
    attr(result[["2026-01-01"]], "tz"),
    "America/New_York"
  )
  expect_identical(
    attr(result[["2026-01-02"]], "tz"),
    "America/New_York"
  )
})


test_that("non-standardized input is rejected", {
  # This input is a data frame but does not contain the required spot column.
  missing_spot <- data.frame(
    datetime = as.POSIXct(
      "2026-01-01 09:30:00",
      tz = "America/New_York"
    ),
    trading_day = as.Date("2026-01-01"),
    futures = 100
  )

  expect_error(
    split_trading_days(missing_spot),
    "must be standardized"
  )

  # This input is not a data frame.
  expect_error(
    split_trading_days(c(1, 2, 3)),
    "must be standardized"
  )
})
