#' Extract the VECM common price
#'
#' @param vecm_fit A fitted `ca.jo` object.
#' @param rank Cointegration rank used by [urca::cajorls()].
#'
#' @return A list containing the common price, residual correlation, and
#'   long-run impact matrix.
#' @export
common_price <- function(vecm_fit, rank = 1L) {
  residuals <- vecm_fit@R0
  cumulative_residuals <- apply(residuals, 2L, cumsum)
  gamma_sum <- .sum_short_run_coefficients(vecm_fit)
  fitted_rank <- urca::cajorls(vecm_fit, r = rank)

  beta_hat <- matrix(fitted_rank$beta[1:2, ], ncol = 1L)
  alpha_hat <- t(stats::coef(fitted_rank$rlm)[1L, ])
  beta_orthogonal <- c(-beta_hat[[2L]], beta_hat[[1L]])
  alpha_orthogonal <- c(-alpha_hat[[2L]], alpha_hat[[1L]])

  denominator <- as.numeric(
    t(alpha_orthogonal) %*%
      (diag(2L) - gamma_sum) %*%
      beta_orthogonal
  )
  if (!is.finite(denominator) || abs(denominator) < .Machine$double.eps^0.5) {
    stop("The estimated common-trend loading matrix is singular.", call. = FALSE)
  }

  long_run_impact <- beta_orthogonal %*% t(alpha_orthogonal) / denominator
  common_price <- as.numeric((cumulative_residuals %*% t(long_run_impact))[, 1L])

  list(
    common_price = common_price,
    residual_correlation = unname(stats::cor(residuals)[2L, 1L]),
    long_run_impact = long_run_impact
  )
}

