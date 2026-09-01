#' Compute Hasbrouck information-share bounds from a VECM
#'
#' @param vecm_fit A fitted `ca.jo` object.
#'
#' @return A named numeric vector for futures and spot.
#' @export
his <- function(vecm_fit) {
  gamma_sum <- .sum_short_run_coefficients(vecm_fit)
  F <- t(chol(vecm_fit@DELTA)) #innovation_factor
  alpha <- vecm_fit@W[, 1L]
  beta <- vecm_fit@V[, 1L]

  beta_orthogonal <- c(-beta[[2L]], beta[[1L]])
  alpha_orthogonal <- c(-alpha[[2L]], alpha[[1L]])
  denominator <- as.numeric(
    t(alpha_orthogonal) %*%
      (diag(2L) - gamma_sum) %*%
      beta_orthogonal
  )
  if (!is.finite(denominator) || abs(denominator) < .Machine$double.eps^0.5) {
    stop("The common-trend loading matrix is singular.", call. = FALSE)
  }

  long_run_impact <- beta_orthogonal %*% t(alpha_orthogonal) / denominator
  xi <- long_run_impact[1L, ]
  numerator <- as.numeric((t(xi) %*% F)^2)
  total <- as.numeric(
    t(xi) %*% F %*% t(F) %*% xi
  )
  if (!is.finite(total) || total <= 0) {
    stop("Cannot normalize the information shares.", call. = FALSE)
  }

  stats::setNames(numerator / total, c("futures", "spot"))
}
