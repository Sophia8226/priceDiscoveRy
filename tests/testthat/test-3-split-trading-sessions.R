test_that("session boundaries reproduce the intended one-second ranges", {
  # These timestamps cover both sides of every session boundary.
  seconds <- c(
    0,     14399,     # 00:00:00 and 03:59:59
    14400, 34199,     # 04:00:00 and 09:29:59
    34200, 57600,     # 09:30:00 and 16:00:00
    57601, 61199,     # 16:00:01 and 16:59:59
    61200, 64799,     # 17:00:00 and 17:59:59
    64800, 71999,     # 18:00:00 and 19:59:59
    72000, 86399      # 20:00:00 and 23:59:59
  )

  origin <- as.POSIXct(
    "2026-01-01 00:00:00",
    tz = "America/New_York"
  )

  input <- data.frame(
    datetime = origin + seconds,
    trading_day = as.Date("2026-01-01"),
    futures = seq_along(seconds) + 100,
    spot = seq_along(seconds) + 200
  )

  # Use non-standard row names to verify that each output is reset.
  rownames(input) <- paste0("row_", seq_len(nrow(input)))

  # Store the time zone in the same way as the validation function.
  attr(input, "tz") <- "America/New_York"

  sessions <- split_trading_sessions(input)

  # The result should contain seven component sessions and one combined overlap.
  expect_named(
    sessions,
    c(
      "futures_pre",
      "overlap_pre",
      "overlap_core",
      "overlap_post_1",
      "spot_maintenance",
      "overlap_post_2",
      "futures_post",
      "overlap"
    )
  )

  # Each component session contains one observation at each end.
  expect_equal(
    vapply(sessions, nrow, integer(1L)),
    c(
      futures_pre = 2L,
      overlap_pre = 2L,
      overlap_core = 2L,
      overlap_post_1 = 2L,
      spot_maintenance = 2L,
      overlap_post_2 = 2L,
      futures_post = 2L,
      overlap = 8L
    )
  )

  # Confirm that each exact boundary is assigned to the correct session.
  expect_equal(
    sessions$futures_pre$futures,
    c(101, 102)
  )
  expect_equal(
    sessions$overlap_pre$futures,
    c(103, 104)
  )
  expect_equal(
    sessions$overlap_core$futures,
    c(105, 106)
  )
  expect_equal(
    sessions$overlap_post_1$futures,
    c(107, 108)
  )
  expect_equal(
    sessions$spot_maintenance$futures,
    c(109, 110)
  )
  expect_equal(
    sessions$overlap_post_2$futures,
    c(111, 112)
  )
  expect_equal(
    sessions$futures_post$futures,
    c(113, 114)
  )

  # The combined overlap excludes the futures-only and maintenance sessions.
  expect_equal(
    sessions$overlap$futures,
    c(103, 104, 105, 106, 107, 108, 111, 112)
  )
})


test_that("16:00:00 belongs to the core session", {
  input <- data.frame(
    datetime = as.POSIXct(
      c(
        "2026-01-01 15:59:59",
        "2026-01-01 16:00:00",
        "2026-01-01 16:00:01"
      ),
      tz = "America/New_York"
    ),
    trading_day = as.Date("2026-01-01"),
    futures = c(100, 101, 102),
    spot = c(200, 201, 202)
  )

  attr(input, "tz") <- "America/New_York"

  sessions <- split_trading_sessions(input)

  # Both 15:59:59 and 16:00:00 remain in the core session.
  expect_equal(
    format(
      sessions$overlap_core$datetime,
      tz = "America/New_York",
      format = "%H:%M:%S"
    ),
    c("15:59:59", "16:00:00")
  )

  # The first post-core overlap begins at 16:00:01.
  expect_equal(
    format(
      sessions$overlap_post_1$datetime,
      tz = "America/New_York",
      format = "%H:%M:%S"
    ),
    "16:00:01"
  )
})


test_that("row names are reset and the time zone is preserved", {
  input <- data.frame(
    datetime = as.POSIXct(
      c(
        "2026-01-01 04:00:00",
        "2026-01-01 04:00:01"
      ),
      tz = "America/New_York"
    ),
    trading_day = as.Date("2026-01-01"),
    futures = c(100, 101),
    spot = c(200, 201)
  )

  rownames(input) <- c("10", "20")
  attr(input, "tz") <- "America/New_York"

  sessions <- split_trading_sessions(input)

  # Old row names should not remain in an extracted session.
  expect_identical(
    rownames(sessions$overlap_pre),
    c("1", "2")
  )

  # The combined overlap should also have clean row names.
  expect_identical(
    rownames(sessions$overlap),
    c("1", "2")
  )

  # The input time zone should be copied to every returned data frame.
  expect_true(
    all(vapply(
      sessions,
      function(x) {
        identical(attr(x, "tz"), "America/New_York")
      },
      logical(1L)
    ))
  )
})


test_that("America/New_York is used when no time-zone attribute is stored", {
  input <- data.frame(
    datetime = as.POSIXct(
      "2026-01-01 04:00:00",
      tz = "America/New_York"
    ),
    trading_day = as.Date("2026-01-01"),
    futures = 100,
    spot = 200
  )

  # No custom "tz" attribute is added to the input.
  sessions <- split_trading_sessions(input)

  # At 04:00:00, the observation should enter the pre-core overlap session.
  expect_equal(nrow(sessions$overlap_pre), 1L)

  # The function should apply its New York fallback time zone.
  expect_identical(
    attr(sessions$overlap_pre, "tz"),
    "America/New_York"
  )
})


test_that("non-standardized daily data are rejected", {
  # This data frame is missing the required spot column.
  missing_spot <- data.frame(
    datetime = as.POSIXct(
      "2026-01-01 04:00:00",
      tz = "America/New_York"
    ),
    trading_day = as.Date("2026-01-01"),
    futures = 100
  )

  expect_error(
    split_trading_sessions(missing_spot),
    "must be a standardized daily data frame",
    fixed = TRUE
  )

  # A numeric vector is not a standardized daily data frame.
  expect_error(
    split_trading_sessions(c(1, 2, 3)),
    "must be a standardized daily data frame",
    fixed = TRUE
  )
})


test_that("invalid session boundaries are rejected", {
  input <- data.frame(
    datetime = as.POSIXct(
      "2026-01-01 04:00:00",
      tz = "America/New_York"
    ),
    trading_day = as.Date("2026-01-01"),
    futures = 100,
    spot = 200
  )

  attr(input, "tz") <- "America/New_York"

  valid_boundaries <- c(
    overlap_open = 4 * 60 * 60,
    core_open = 9 * 60 * 60 + 30 * 60,
    core_close_exclusive = 16 * 60 * 60 + 1,
    maintenance_start = 17 * 60 * 60,
    overlap_resume = 18 * 60 * 60,
    overlap_close = 20 * 60 * 60
  )

  # Boundary names are required and must match the default names.
  expect_error(
    split_trading_sessions(
      input,
      boundaries = unname(valid_boundaries)
    ),
    "must be an increasing named vector",
    fixed = TRUE
  )

  wrong_names <- valid_boundaries
  names(wrong_names)[[1L]] <- "wrong_name"

  expect_error(
    split_trading_sessions(
      input,
      boundaries = wrong_names
    ),
    "must be an increasing named vector",
    fixed = TRUE
  )

  # Boundaries must be strictly increasing.
  non_increasing <- valid_boundaries
  non_increasing[["core_open"]] <-
    non_increasing[["overlap_open"]]

  expect_error(
    split_trading_sessions(
      input,
      boundaries = non_increasing
    ),
    "must be an increasing named vector",
    fixed = TRUE
  )

  # The final boundary must remain within the same 24-hour day.
  outside_day <- valid_boundaries
  outside_day[["overlap_close"]] <- 24 * 60 * 60

  expect_error(
    split_trading_sessions(
      input,
      boundaries = outside_day
    ),
    "must be an increasing named vector",
    fixed = TRUE
  )
})
