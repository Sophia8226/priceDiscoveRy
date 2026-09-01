#' Estimate realized variance from efficient-price returns
#'
#' @param returns Numeric efficient-price returns.
#'
#' @return Sum of squared returns.
#' @export
realized_variance <- function(returns) {
  if (!is.numeric(returns) || any(!is.finite(returns))) {
    stop("`returns` must be a finite numeric vector.", call. = FALSE)
  }
  sum(returns^2)
}
