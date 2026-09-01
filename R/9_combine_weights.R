#' Combine period estimates and calculate variance weights
#'
#' @param overlapping_variance Named realized variances from overlapping
#'   sessions.
#' @param non_overlapping_variance Named realized kernels from single-market
#'   sessions.
#'
#' @return A list containing period variances, variance weights, and the total
#'   overlapping-period weight.
#' @export
combine_weights <- function(
    overlapping_variance,
    non_overlapping_variance) {
  all_variance <- c(non_overlapping_variance, overlapping_variance)
  if (is.null(names(all_variance)) || anyDuplicated(names(all_variance))) {
    stop("All period estimates must have unique names.", call. = FALSE)
  }
  if (any(!is.finite(all_variance)) || any(all_variance < 0)) {
    stop("Period variance estimates must be finite and non-negative.", call. = FALSE)
  }

  total <- sum(all_variance)
  if (total <= 0) {
    stop("The total realized variance must be positive.", call. = FALSE)
  }
  weights <- all_variance / total
  overlap_names <- names(overlapping_variance)

  list(
    realized_variance = all_variance,
    variance_weights = weights,
    overlapping_weight = sum(weights[overlap_names])
  )
}
