# Create reproducible cointegrated log-price series for common-price tests.
.make_common_price_test_log_prices <- function(
    n = 300L,
    seed = 123L) {
  set.seed(seed)

  # Generate a common stochastic trend shared by both markets.
  common_trend <- 7 + cumsum(
    stats::rnorm(
      n,
      mean = 0,
      sd = 0.002
    )
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


test_that("common_price returns the expected result structure", {
  log_prices <- .make_common_price_test_log_prices()

  fit <- vecm(
    log_prices = log_prices,
    K = 3L
  )

  result <- common_price(
    vecm_fit = fit,
    rank = 1L
  )

  # The function should return a list with three named components.
  expect_type(result, "list")
  expect_named(
    result,
    c(
      "common_price",
      "residual_correlation",
      "long_run_impact"
    )
  )

  # The common price should be a finite numeric vector.
  expect_true(is.numeric(result$common_price))
  expect_true(all(is.finite(result$common_price)))

  # The common-price length should match the number of VECM residuals.
  expect_length(
    result$common_price,
    nrow(fit@R0)
  )

  # The residual correlation should be one finite scalar.
  expect_true(is.numeric(result$residual_correlation))
  expect_length(result$residual_correlation, 1L)
  expect_true(is.finite(result$residual_correlation))

  # A correlation coefficient must lie between minus one and one.
  expect_true(result$residual_correlation >= -1)
  expect_true(result$residual_correlation <= 1)

  # The long-run impact matrix should be a finite 2-by-2 matrix.
  expect_true(is.matrix(result$long_run_impact))
  expect_equal(
    dim(result$long_run_impact),
    c(2L, 2L)
  )
  expect_true(all(is.finite(result$long_run_impact)))
})


test_that("common_price rejects a singular long-run loading matrix", {
  log_prices <- .make_common_price_test_log_prices(
    n = 300L,
    seed = 654L
  )

  # K = 2 produces one differenced-lag coefficient block.
  fit <- vecm(
    log_prices = log_prices,
    K = 2L
  )

  # Work with a copy so that the original fitted object is not changed.
  singular_fit <- fit

  # Force the summed short-run coefficient matrix to equal the identity matrix.
  # This makes I - Gamma.sum equal to a zero matrix.
  singular_fit@GAMMA[, 2:3] <- diag(2L)

  expect_error(
    common_price(singular_fit),
    "The estimated common-trend loading matrix is singular.",
    fixed = TRUE
  )
})
