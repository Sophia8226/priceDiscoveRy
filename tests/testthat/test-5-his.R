# Create reproducible cointegrated log-price series for HIS tests.
.make_his_test_log_prices <- function(n = 300L, seed = 123L) {
  set.seed(seed)

  # Generate a common stochastic trend shared by both markets.
  common_trend <- 7 + cumsum(
    stats::rnorm(n, mean = 0, sd = 0.002)
  )

  # Generate a stationary spread between futures and spot prices.
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


test_that("his returns finite and normalized information shares", {
  log_prices <- .make_his_test_log_prices()

  fit <- vecm(
    log_prices = log_prices,
    K = 3L
  )

  shares <- his(fit)

  # The result should be a named numeric vector for the two markets.
  expect_true(is.numeric(shares))
  expect_length(shares, 2L)
  expect_named(
    shares,
    c("futures", "spot")
  )

  # Both information shares should be finite.
  expect_true(all(is.finite(shares)))

  # Squared innovation contributions should produce non-negative shares.
  expect_true(all(shares >= 0))

  # A normalized information share should not exceed one.
  expect_true(all(shares <= 1))

  # Futures and spot information shares should sum to one.
  expect_equal(
    sum(shares),
    1,
    tolerance = 1e-10
  )
})


test_that("his rejects a singular common-trend loading matrix", {
  log_prices <- .make_his_test_log_prices(
    n = 300L,
    seed = 789L
  )

  fit <- vecm(
    log_prices = log_prices,
    K = 3L
  )

  # Set the adjustment vector to zero to force a zero denominator.
  singular_fit <- fit
  singular_fit@W[, 1L] <- 0

  expect_error(
    his(singular_fit),
    "The common-trend loading matrix is singular.",
    fixed = TRUE
  )
})
