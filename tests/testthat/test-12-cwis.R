# Load selected dates from the package example data.
.load_cwis_test_dates <- function(dates) {
  data_name <-
    "example_sp500_futures_spot_oneweek_24h_1sec"

  data_environment <- new.env(
    parent = baseenv()
  )

  # Load the bundled example data into an isolated environment.
  utils::data(
    list = data_name,
    package = "priceDiscoveRy",
    envir = data_environment
  )

  raw_data <- get(
    data_name,
    envir = data_environment
  )

  # Convert data.table input to a regular data frame.
  raw_data <- as.data.frame(raw_data)

  available_dates <- unique(raw_data$date)

  if (!all(dates %in% available_dates)) {
    stop(
      "One or more requested test dates are not available.",
      call. = FALSE
    )
  }

  # Keep only the dates required by the test.
  raw_data[
    raw_data$date %in% dates,
    ,
    drop = FALSE
  ]
}


test_that("cwis integrates the complete multi-day workflow", {
  raw_data <- .load_cwis_test_dates(
    c(
      "2019-06-24", # A complete weekday
      "2019-06-30" # An incomplete Sunday with no futures_post session.
    )
  )

  # Run one successful day and one expected failure in a single call.
  result <- cwis(
    data = raw_data,
    datetime_col = "datetime",
    futures_col = "V1",
    spot_col = "V2",
    spot_multiplier = 10,
    tz = "America/New_York",
    K = 10L,
    kernel_type = "ModifiedTukeyHanning",
    bandwidth_constant = NULL,
    align_by = "seconds",
    align_period = 1L
  )

  # Check the top-level class and structure.
  expect_s3_class(
    result,
    "cwis_result"
  )
  expect_named(
    result,
    c(
      "daily",
      "summary",
      "diagnostics",
      "variance_weights",
      "details",
      "settings"
    )
  )

  # Check successful and failed daily records.
  expect_equal(
    result$daily$date,
    as.Date(
      c(
        "2019-06-24",
        "2019-06-30"
      )
    )
  )
  expect_equal(
    result$daily$status,
    c("ok", "failed")
  )
  expect_true(
    is.na(result$daily$error[[1L]])
  )
  expect_true(
    grepl(
      "futures_post",
      result$daily$error[[2L]],
      fixed = TRUE
    )
  )

  # Check the detailed daily records.
  expect_named(
    result$details,
    c(
      "2019-06-24",
      "2019-06-30"
    )
  )

  successful_day <-
    result$details[["2019-06-24"]]

  failed_day <-
    result$details[["2019-06-30"]]

  expect_s3_class(
    successful_day,
    "cwis_day_result"
  )
  expect_false(
    inherits(
      failed_day,
      "cwis_day_result"
    )
  )
  expect_equal(
    failed_day$date,
    as.Date("2019-06-30")
  )
  expect_true(
    grepl(
      "futures_post",
      failed_day$error,
      fixed = TRUE
    )
  )

  # Check normalized successful-day results.
  expect_equal(
    sum(successful_day$his_midpoint),
    1,
    tolerance = 1e-8
  )
  expect_equal(
    sum(successful_day$variance_weights),
    1,
    tolerance = 1e-8
  )
  expect_equal(
    sum(successful_day$cwis),
    1,
    tolerance = 1e-8
  )

  # Check aggregated day counts.
  diagnostic_values <- stats::setNames(
    result$diagnostics$value,
    result$diagnostics$statistic
  )

  expect_equal(
    diagnostic_values[["days supplied"]],
    2
  )
  expect_equal(
    diagnostic_values[["days successful"]],
    1
  )
  expect_equal(
    diagnostic_values[["days failed"]],
    1
  )

  # With one successful day, summary means equal that day's estimates.
  expect_equal(
    result$summary$futures[
      result$summary$statistic == "HIS mean"
    ],
    successful_day$his_midpoint[["futures"]],
    tolerance = 1e-10
  )
  expect_equal(
    result$summary$spot[
      result$summary$statistic == "HIS mean"
    ],
    successful_day$his_midpoint[["spot"]],
    tolerance = 1e-10
  )
  expect_equal(
    result$summary$futures[
      result$summary$statistic == "CWIS mean"
    ],
    successful_day$cwis[["futures"]],
    tolerance = 1e-10
  )
  expect_equal(
    result$summary$spot[
      result$summary$statistic == "CWIS mean"
    ],
    successful_day$cwis[["spot"]],
    tolerance = 1e-10
  )

  # Cross-day standard deviations are undefined with one successful day.
  sd_rows <- result$summary$statistic %in%
    c("HIS SD", "CWIS SD")

  expect_true(
    all(
      is.na(
        result$summary[
          sd_rows,
          c("futures", "spot"),
          drop = FALSE
        ]
      )
    )
  )

  # Check settings required for reproducibility.
  expect_identical(
    result$settings$datetime_col,
    "datetime"
  )
  expect_identical(
    result$settings$futures_col,
    "V1"
  )
  expect_identical(
    result$settings$spot_col,
    "V2"
  )
  expect_equal(
    result$settings$spot_multiplier,
    10
  )
  expect_identical(
    result$settings$tz,
    "America/New_York"
  )
  expect_equal(
    result$settings$K,
    10L
  )
  expect_identical(
    result$settings$kernel_type,
    "ModifiedTukeyHanning"
  )
})


test_that("continue_on_error FALSE propagates a daily error", {
  incomplete_day <- .load_cwis_test_dates(
    "2019-06-30"
  )

  # The incomplete day should stop immediately when error continuation is off.
  expect_error(
    cwis(
      data = incomplete_day,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      spot_multiplier = 10,
      tz = "America/New_York",
      K = 10L,
      continue_on_error = FALSE
    ),
    "Insufficient observations in sessions: futures_post",
    fixed = TRUE
  )
})


test_that("input validation errors stop before daily processing", {
  invalid_data <- data.frame(
    datetime = c(
      "2026-01-04 00:00:00",
      "2026-01-04 00:00:01"
    ),
    V1 = c(3700, 3701)
  )

  # continue_on_error applies only to daily estimation errors.
  expect_error(
    cwis(
      data = invalid_data,
      datetime_col = "datetime",
      futures_col = "V1",
      spot_col = "V2",
      spot_multiplier = 10,
      tz = "America/New_York",
      K = 10L,
      continue_on_error = TRUE
    ),
    "Missing required columns: V2",
    fixed = TRUE
  )
})


test_that("print.cwis_result produces a compact summary", {
  print_test_object <- structure(
    list(
      daily = data.frame(
        status = c(
          "ok",
          "ok",
          "failed"
        )
      ),
      summary = data.frame(
        statistic = c(
          "HIS mean",
          "CWIS mean"
        ),
        futures = c(0.45, 0.55),
        spot = c(0.55, 0.45)
      )
    ),
    class = "cwis_result"
  )

  # Capture console output generated by the S3 print method.
  output <- capture.output(
    returned_object <- print(
      print_test_object
    )
  )

  # Combine all output lines into one character string.
  printed_text <- paste(
    output,
    collapse = "\n"
  )

  # Check the printed result title.
  expect_true(
    grepl(
      "24-hour CWIS result",
      printed_text,
      fixed = TRUE
    )
  )

  # Check both the label and the number of successful days.
  expect_match(
    printed_text,
    "Successful days:\\s*2"
  )

  # Check both the label and the number of failed days.
  expect_match(
    printed_text,
    "Failed days:\\s*1"
  )

  # The print method should return its original input object.
  expect_identical(
    returned_object,
    print_test_object
  )
})
