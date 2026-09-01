test_that("realized variance equals the sum of squared returns", {
  returns <- c(1, -2, 3)

  result <- realized_variance(returns)

  # The function should return one numeric value.
  expect_true(is.numeric(result))
  expect_length(result, 1L)

  # Realized variance is 1^2 + (-2)^2 + 3^2 = 14.
  expect_equal(result, 14)
})


test_that("non-numeric returns are rejected", {
  character_returns <- c("0.01", "-0.02", "0.03")

  expect_error(
    realized_variance(character_returns),
    "`returns` must be a finite numeric vector.",
    fixed = TRUE
  )

  factor_returns <- factor(
    c("0.01", "-0.02", "0.03")
  )

  expect_error(
    realized_variance(factor_returns),
    "`returns` must be a finite numeric vector.",
    fixed = TRUE
  )
})


test_that("missing and non-finite returns are rejected", {
  returns_with_na <- c(0.01, NA_real_, 0.03)
  returns_with_nan <- c(0.01, NaN, 0.03)
  returns_with_inf <- c(0.01, Inf, 0.03)
  returns_with_negative_inf <- c(0.01, -Inf, 0.03)

  # Missing returns should not be silently removed.
  expect_error(
    realized_variance(returns_with_na),
    "`returns` must be a finite numeric vector.",
    fixed = TRUE
  )

  # NaN is not a valid finite return.
  expect_error(
    realized_variance(returns_with_nan),
    "`returns` must be a finite numeric vector.",
    fixed = TRUE
  )

  # Positive infinity is not a valid finite return.
  expect_error(
    realized_variance(returns_with_inf),
    "`returns` must be a finite numeric vector.",
    fixed = TRUE
  )

  # Negative infinity is not a valid finite return.
  expect_error(
    realized_variance(returns_with_negative_inf),
    "`returns` must be a finite numeric vector.",
    fixed = TRUE
  )
})
