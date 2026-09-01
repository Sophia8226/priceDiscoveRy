# Create reproducible cointegrated log-price series for VECM tests.
.make_test_log_prices <- function(n = 250L, seed = 123L) {
  set.seed(seed)

  # Generate a common stochastic trend.
  common_trend <- 7 + cumsum(
    stats::rnorm(n, mean = 0, sd = 0.002)
  )

  # Generate a stationary spread around the common trend.
  stationary_spread <- as.numeric(
    stats::arima.sim(
      model = list(ar = 0.6),
      n = n,
      sd = 0.001
    )
  )

  cbind(
    futures = common_trend + stationary_spread / 2,
    spot = common_trend - stationary_spread / 2
  )
}


test_that("vecm returns a ca.jo object with the requested settings", {
  log_prices <- .make_test_log_prices()

  # Use a data frame to verify that the function also accepts data-frame input.
  fit <- vecm(
    log_prices = as.data.frame(log_prices),
    K = 3L,
    type = "trace",
    ecdet = "none",
    spec = "transitory"
  )

  # The fitted model should be a formal ca.jo object from the urca package.
  expect_s4_class(fit, "ca.jo")

  # The requested lag order should be stored in the fitted object.
  expect_equal(fit@lag, 3L)

  # The wrapper should assign the expected market names.
  expect_identical(
    colnames(fit@x),
    c("futures", "spot")
  )

  # The model settings should be passed to urca::ca.jo().
  expect_match(fit@type, "trace")
  expect_identical(fit@ecdet, "none")
  expect_identical(fit@spec, "transitory")

  # The estimated test statistics should be finite.
  expect_true(all(is.finite(fit@teststat)))
})


test_that("vecm accepts a numeric two-column matrix", {
  log_prices <- .make_test_log_prices(
    n = 200L,
    seed = 456L
  )

  fit <- vecm(
    log_prices = log_prices,
    K = 2L
  )

  expect_s4_class(fit, "ca.jo")
  expect_equal(fit@lag, 2L)
})


test_that("vecm rejects non-numeric input", {
  input <- data.frame(
    futures = as.character(seq_len(30L)),
    spot = as.character(seq_len(30L))
  )

  expect_error(
    vecm(
      log_prices = input,
      K = 2L
    ),
    "`log_prices` must be a numeric two-column object.",
    fixed = TRUE
  )
})


test_that("vecm requires exactly two price columns", {
  one_column <- matrix(
    seq_len(30L),
    ncol = 1L
  )

  three_columns <- matrix(
    stats::rnorm(90L),
    ncol = 3L
  )

  expect_error(
    vecm(
      log_prices = one_column,
      K = 2L
    ),
    "numeric two-column object",
    fixed = TRUE
  )

  expect_error(
    vecm(
      log_prices = three_columns,
      K = 2L
    ),
    "numeric two-column object",
    fixed = TRUE
  )
})


test_that("vecm rejects insufficient observations", {
  # With K = 2, the function requires more than K + 2 observations.
  too_short <- matrix(
    c(
      1.00, 1.01,
      1.01, 1.02,
      1.02, 1.03,
      1.03, 1.04
    ),
    ncol = 2L,
    byrow = TRUE
  )

  expect_error(
    vecm(
      log_prices = too_short,
      K = 2L
    ),
    "too few complete observations",
    fixed = TRUE
  )
})


test_that("vecm rejects non-finite observations", {
  log_prices <- .make_test_log_prices(
    n = 100L,
    seed = 789L
  )

  log_prices[10L, "futures"] <- NA_real_

  expect_error(
    vecm(
      log_prices = log_prices,
      K = 2L
    ),
    "too few complete observations",
    fixed = TRUE
  )

  log_prices <- .make_test_log_prices(
    n = 100L,
    seed = 789L
  )

  log_prices[10L, "spot"] <- Inf

  expect_error(
    vecm(
      log_prices = log_prices,
      K = 2L
    ),
    "too few complete observations",
    fixed = TRUE
  )
})


test_that("both log-price series must vary", {
  input <- cbind(
    futures = rep(7, 100L),
    spot = 7 + cumsum(rep(0.001, 100L))
  )

  expect_error(
    vecm(
      log_prices = input,
      K = 2L
    ),
    "Both log-price series must vary within the overlapping period.",
    fixed = TRUE
  )
})


test_that("short-run coefficient blocks are summed correctly", {
  log_prices <- .make_test_log_prices(
    n = 250L,
    seed = 321L
  )

  # K = 3 produces two differenced-lag coefficient blocks.
  fit <- vecm(
    log_prices = log_prices,
    K = 3L
  )

  # Remove the first GAMMA column in the same way required by the model layout.
  gamma <- fit@GAMMA[, -1L, drop = FALSE]

  # For two variables, each lag contributes one two-column coefficient block.
  first_lag <- gamma[, 1:2, drop = FALSE]
  second_lag <- gamma[, 3:4, drop = FALSE]
  expected_sum <- first_lag + second_lag

  # The triple colon accesses an internal function only for package testing.
  actual_sum <-
    priceDiscoveRy:::.sum_short_run_coefficients(fit)

  expect_equal(dim(actual_sum), c(2L, 2L))
  expect_equal(actual_sum, expected_sum)
})
