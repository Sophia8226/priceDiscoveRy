test_that("period variances are normalized into weights", {
  overlapping_variance <- c(
    overlap_pre = 2,
    overlap_core = 3,
    overlap_post = 5
  )

  non_overlapping_variance <- c(
    futures_pre = 1,
    spot_maintenance = 4,
    futures_post = 5
  )

  result <- combine_weights(
    overlapping_variance = overlapping_variance,
    non_overlapping_variance = non_overlapping_variance
  )

  # The total variance is 20.
  expect_equal(
    result$realized_variance,
    c(
      futures_pre = 1,
      spot_maintenance = 4,
      futures_post = 5,
      overlap_pre = 2,
      overlap_core = 3,
      overlap_post = 5
    )
  )

  expect_equal(
    result$variance_weights,
    c(
      futures_pre = 0.05,
      spot_maintenance = 0.20,
      futures_post = 0.25,
      overlap_pre = 0.10,
      overlap_core = 0.15,
      overlap_post = 0.25
    )
  )

  # All period weights should sum to one.
  expect_equal(
    sum(result$variance_weights),
    1
  )

  # All three overlap periods contribute 10 / 20 = 0.5.
  expect_equal(
    result$overlapping_weight,
    0.5
  )
})


test_that("zero period variances are allowed when total variance is positive", {
  overlapping_variance <- c(
    overlap_pre = 0,
    overlap_core = 2
  )

  non_overlapping_variance <- c(
    futures_pre = 0,
    spot_maintenance = 2
  )

  result <- combine_weights(
    overlapping_variance = overlapping_variance,
    non_overlapping_variance = non_overlapping_variance
  )

  # A zero variance period should receive a zero weight.
  expect_equal(
    result$variance_weights[["futures_pre"]],
    0
  )

  expect_equal(
    result$variance_weights[["overlap_pre"]],
    0
  )

  # Positive periods should still be normalized by the positive total.
  expect_equal(
    result$variance_weights[["spot_maintenance"]],
    0.5
  )

  expect_equal(
    result$variance_weights[["overlap_core"]],
    0.5
  )

  # The overlap weight contains only the positive overlap-core contribution.
  expect_equal(
    result$overlapping_weight,
    0.5
  )

  expect_equal(
    sum(result$variance_weights),
    1
  )
})


test_that("period estimates must have names", {
  overlapping_variance <- c(2, 3)
  non_overlapping_variance <- c(1, 4)

  expect_error(
    combine_weights(
      overlapping_variance = overlapping_variance,
      non_overlapping_variance = non_overlapping_variance
    ),
    "All period estimates must have unique names.",
    fixed = TRUE
  )
})


test_that("period estimate names must be unique", {
  overlapping_variance <- c(
    overlap_pre = 2,
    overlap_core = 3
  )

  # This name duplicates one of the overlapping-period names.
  non_overlapping_variance <- c(
    futures_pre = 1,
    overlap_pre = 4
  )

  expect_error(
    combine_weights(
      overlapping_variance = overlapping_variance,
      non_overlapping_variance = non_overlapping_variance
    ),
    "All period estimates must have unique names.",
    fixed = TRUE
  )

  # Duplicate names inside one input vector should also be rejected.
  duplicated_overlap <- c(2, 3)
  names(duplicated_overlap) <- c(
    "overlap_pre",
    "overlap_pre"
  )

  expect_error(
    combine_weights(
      overlapping_variance = duplicated_overlap,
      non_overlapping_variance = c(futures_pre = 1)
    ),
    "All period estimates must have unique names.",
    fixed = TRUE
  )
})


test_that("non-finite period variances are rejected", {
  valid_non_overlap <- c(
    futures_pre = 1,
    spot_maintenance = 4
  )

  overlap_with_na <- c(
    overlap_pre = NA_real_,
    overlap_core = 3
  )

  expect_error(
    combine_weights(
      overlapping_variance = overlap_with_na,
      non_overlapping_variance = valid_non_overlap
    ),
    "Period variance estimates must be finite and non-negative.",
    fixed = TRUE
  )

  overlap_with_nan <- c(
    overlap_pre = NaN,
    overlap_core = 3
  )

  expect_error(
    combine_weights(
      overlapping_variance = overlap_with_nan,
      non_overlapping_variance = valid_non_overlap
    ),
    "Period variance estimates must be finite and non-negative.",
    fixed = TRUE
  )

  overlap_with_inf <- c(
    overlap_pre = Inf,
    overlap_core = 3
  )

  expect_error(
    combine_weights(
      overlapping_variance = overlap_with_inf,
      non_overlapping_variance = valid_non_overlap
    ),
    "Period variance estimates must be finite and non-negative.",
    fixed = TRUE
  )

  overlap_with_negative_inf <- c(
    overlap_pre = -Inf,
    overlap_core = 3
  )

  expect_error(
    combine_weights(
      overlapping_variance = overlap_with_negative_inf,
      non_overlapping_variance = valid_non_overlap
    ),
    "Period variance estimates must be finite and non-negative.",
    fixed = TRUE
  )
})


test_that("negative period variances are rejected", {
  overlapping_variance <- c(
    overlap_pre = -1,
    overlap_core = 3
  )

  non_overlapping_variance <- c(
    futures_pre = 1,
    spot_maintenance = 4
  )

  expect_error(
    combine_weights(
      overlapping_variance = overlapping_variance,
      non_overlapping_variance = non_overlapping_variance
    ),
    "Period variance estimates must be finite and non-negative.",
    fixed = TRUE
  )
})


test_that("total realized variance must be positive", {
  overlapping_variance <- c(
    overlap_pre = 0,
    overlap_core = 0
  )

  non_overlapping_variance <- c(
    futures_pre = 0,
    spot_maintenance = 0
  )

  expect_error(
    combine_weights(
      overlapping_variance = overlapping_variance,
      non_overlapping_variance = non_overlapping_variance
    ),
    "The total realized variance must be positive.",
    fixed = TRUE
  )
})
