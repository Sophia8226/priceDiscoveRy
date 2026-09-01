# Create a small standardized session for realized-kernel tests.
.make_kernel_test_session <- function(n = 120L) {
  datetime <- as.POSIXct(
    "2021-01-04 04:00:00",
    tz = "America/New_York"
  ) + seq.int(0L, n - 1L)

  # Generate deterministic and strictly positive futures prices.
  futures_returns <- 0.0001 *
    sin(seq_len(n) / 5)

  futures <- 3700 * exp(
    cumsum(futures_returns)
  )

  # Generate a different deterministic spot-price path.
  spot_returns <- 0.00012 *
    cos(seq_len(n) / 7)

  spot <- 370 * exp(
    cumsum(spot_returns)
  )

  session <- data.frame(
    datetime = datetime,
    trading_day = as.Date("2021-01-04"),
    futures = futures,
    spot = spot
  )

  attr(session, "tz") <- "America/New_York"
  session
}


test_that("realized_kernel returns one finite non-negative value", {
  session <- .make_kernel_test_session()

  result <- realized_kernel(
    session_data = session,
    price_col = "futures"
  )

  # A univariate realized kernel should return one numeric value.
  expect_true(is.numeric(result))
  expect_length(result, 1L)

  # The estimate should be finite and non-negative.
  expect_true(is.finite(result))
  expect_true(result >= 0)
})


test_that("Modified Tukey-Hanning uses the documented default bandwidth", {
  session <- .make_kernel_test_session(
    n = 120L
  )

  actual_result <- realized_kernel(
    session_data = session,
    price_col = "futures",
    kernel_type = "ModifiedTukeyHanning",
    bandwidth_constant = NULL,
    align_by = "seconds",
    align_period = 1L
  )

  # Reproduce the documented bandwidth rule.
  expected_parameter <- as.integer(
    2.3970 * sqrt(nrow(session))
  )

  # Reproduce the direct highfrequency call used by the wrapper.
  price_xts <- xts::xts(
    session$futures,
    order.by = session$datetime
  )

  expected_result <- highfrequency::rKernelCov(
    rData = price_xts,
    align.by = "seconds",
    align.period = 1L,
    makeReturns = TRUE,
    kernel.type = "ModifiedTukeyHanning",
    kernel.param = expected_parameter
  )

  expected_result <- unname(
    as.numeric(expected_result)[[1L]]
  )

  # The wrapper result should match the direct package call.
  expect_equal(
    actual_result,
    expected_result,
    tolerance = 1e-12
  )
})


test_that("Parzen uses the documented default bandwidth", {
  session <- .make_kernel_test_session(
    n = 120L
  )

  actual_result <- realized_kernel(
    session_data = session,
    price_col = "spot",
    kernel_type = "Parzen",
    bandwidth_constant = NULL,
    align_by = "seconds",
    align_period = 1L
  )

  # Reproduce the Parzen rule-of-thumb bandwidth.
  expected_parameter <- as.integer(
    3.5134 * sqrt(nrow(session))
  )

  # Construct the spot-price xts series used by rKernelCov().
  price_xts <- xts::xts(
    session$spot,
    order.by = session$datetime
  )

  expected_result <- highfrequency::rKernelCov(
    rData = price_xts,
    align.by = "seconds",
    align.period = 1L,
    makeReturns = TRUE,
    kernel.type = "Parzen",
    kernel.param = expected_parameter
  )

  expected_result <- unname(
    as.numeric(expected_result)[[1L]]
  )

  expect_equal(
    actual_result,
    expected_result,
    tolerance = 1e-12
  )
})


test_that("a supplied bandwidth constant overrides the default rule", {
  session <- .make_kernel_test_session(
    n = 120L
  )

  bandwidth_constant <- 1.5

  actual_result <- realized_kernel(
    session_data = session,
    price_col = "futures",
    kernel_type = "ModifiedTukeyHanning",
    bandwidth_constant = bandwidth_constant,
    align_by = "seconds",
    align_period = 1L
  )

  # Calculate the parameter from the user-supplied constant.
  expected_parameter <- as.integer(
    bandwidth_constant * sqrt(nrow(session))
  )

  price_xts <- xts::xts(
    session$futures,
    order.by = session$datetime
  )

  expected_result <- highfrequency::rKernelCov(
    rData = price_xts,
    align.by = "seconds",
    align.period = 1L,
    makeReturns = TRUE,
    kernel.type = "ModifiedTukeyHanning",
    kernel.param = expected_parameter
  )

  expected_result <- unname(
    as.numeric(expected_result)[[1L]]
  )

  expect_equal(
    actual_result,
    expected_result,
    tolerance = 1e-12
  )
})


test_that("price_col must identify one supported market", {
  session <- .make_kernel_test_session()

  # The original input name V1 is not valid after standardization.
  expect_error(
    realized_kernel(
      session_data = session,
      price_col = "V1"
    ),
    "`price_col` must be either \"futures\" or \"spot\".",
    fixed = TRUE
  )

  # Only one market may be selected at a time.
  expect_error(
    realized_kernel(
      session_data = session,
      price_col = c("futures", "spot")
    ),
    "`price_col` must be either \"futures\" or \"spot\".",
    fixed = TRUE
  )

  # A numeric column identifier is not accepted.
  expect_error(
    realized_kernel(
      session_data = session,
      price_col = 1L
    ),
    "`price_col` must be either \"futures\" or \"spot\".",
    fixed = TRUE
  )
})


test_that("session data must contain datetime and the selected price series", {
  session <- .make_kernel_test_session()

  # Remove the datetime column.
  missing_datetime <- session[
    ,
    setdiff(names(session), "datetime"),
    drop = FALSE
  ]

  expect_error(
    realized_kernel(
      session_data = missing_datetime,
      price_col = "futures"
    ),
    "`session_data` does not contain the requested price series.",
    fixed = TRUE
  )

  # Remove the selected spot-price column.
  missing_spot <- session[
    ,
    setdiff(names(session), "spot"),
    drop = FALSE
  ]

  expect_error(
    realized_kernel(
      session_data = missing_spot,
      price_col = "spot"
    ),
    "`session_data` does not contain the requested price series.",
    fixed = TRUE
  )

  # A matrix does not satisfy the standardized data-frame contract.
  matrix_input <- as.matrix(
    session[, c("futures", "spot")]
  )

  expect_error(
    realized_kernel(
      session_data = matrix_input,
      price_col = "futures"
    ),
    "`session_data` does not contain the requested price series.",
    fixed = TRUE
  )
})


test_that("a session needs at least two observations", {
  session <- .make_kernel_test_session()

  one_observation <- session[
    1L,
    ,
    drop = FALSE
  ]

  expect_error(
    realized_kernel(
      session_data = one_observation,
      price_col = "futures"
    ),
    "A single-market session needs at least two observations.",
    fixed = TRUE
  )
})


test_that("kernel prices must be finite and strictly positive", {
  session_with_na <- .make_kernel_test_session()
  session_with_na$futures[[10L]] <- NA_real_

  expect_error(
    realized_kernel(
      session_data = session_with_na,
      price_col = "futures"
    ),
    "Kernel prices must be finite and strictly positive.",
    fixed = TRUE
  )

  session_with_inf <- .make_kernel_test_session()
  session_with_inf$futures[[10L]] <- Inf

  expect_error(
    realized_kernel(
      session_data = session_with_inf,
      price_col = "futures"
    ),
    "Kernel prices must be finite and strictly positive.",
    fixed = TRUE
  )

  session_with_zero <- .make_kernel_test_session()
  session_with_zero$futures[[10L]] <- 0

  expect_error(
    realized_kernel(
      session_data = session_with_zero,
      price_col = "futures"
    ),
    "Kernel prices must be finite and strictly positive.",
    fixed = TRUE
  )

  session_with_negative <- .make_kernel_test_session()
  session_with_negative$futures[[10L]] <- -1

  expect_error(
    realized_kernel(
      session_data = session_with_negative,
      price_col = "futures"
    ),
    "Kernel prices must be finite and strictly positive.",
    fixed = TRUE
  )
})


test_that("unknown kernels require an explicit bandwidth constant", {
  session <- .make_kernel_test_session()

  # An unknown kernel has no package-defined rule-of-thumb constant.
  expect_error(
    realized_kernel(
      session_data = session,
      price_col = "futures",
      kernel_type = "UnknownKernel",
      bandwidth_constant = NULL
    ),
    "Supply `bandwidth_constant` for kernel type UnknownKernel.",
    fixed = TRUE
  )
})
