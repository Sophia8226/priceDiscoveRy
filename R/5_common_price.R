#' Extract the common price from a VECM
#'
#' @description
#' Extracts a cumulative permanent common-price component from a fitted
#' bivariate VECM using its common-trend representation. The function also
#' returns the long-run impact matrix and the correlation between the two
#' residual series used in the calculation.
#'
#' @details
#' The function is motivated by the "one security - many markets" framework
#' of Hasbrouck (1995). Under this framework, an asset has a single latent
#' efficient price even when it is traded in multiple markets. Observed prices
#' may temporarily deviate from this fundamental value because of
#' market-microstructure effects.
#'
#' Let a trading day \eqn{d} consist of \eqn{T} equally spaced observations
#' indexed by \eqn{t}. The day is divided into \eqn{H} disjoint intraday
#' intervals,
#'
#' \deqn{
#' S_h =
#' \left\{
#' t \mid
#' t^\circ_{h-1} < t \leq t^\circ_h
#' \right\},
#' \qquad h=1,\ldots,H,
#' }
#'
#' where \eqn{t^\circ_h} denotes the final observation in interval \eqn{S_h},
#' with \eqn{t^\circ_0=0} and \eqn{t^\circ_H=T}. Together, the intervals
#' \eqn{S_1,\ldots,S_H} cover the entire trading day.
#'
#' The latent efficient price is unobserved and is assumed to evolve as a
#' random walk irrespective of how the trading day is partitioned:
#'
#' \deqn{
#' m_{d,t} = m_{d,t-1} + e_{d,t},
#' }
#'
#' where \eqn{e_{d,t}} is an independent mean-zero innovation. The innovations
#' need not be identically distributed, allowing the variance of the efficient
#' price to differ across intraday intervals.
#'
#' When futures and spot markets trade simultaneously, their observed prices
#' may temporarily differ because of market-microstructure effects. Under the
#' law of one price, however, the two log-price series are expected to share a
#' common long-run stochastic trend. Their joint long-run and short-run
#' dynamics can therefore be represented by a bivariate VECM.
#'
#' Let the bivariate VECM be written, for the transitory specification, as
#'
#' \deqn{
#' \Delta p_t =
#' \Pi p_{t-1} +
#' \sum_{i=1}^{K-1}\Gamma_i\Delta p_{t-i} +
#' d_t + u_t,
#' }
#'
#' where \eqn{p_t} contains the futures and spot log prices,
#' \eqn{\Pi = \alpha\beta^\prime} is the long-run coefficient matrix,
#' \eqn{\beta} contains the cointegrating vector, \eqn{\alpha} contains the
#' adjustment coefficients, \eqn{\Gamma_i} contains the short-run coefficient
#' matrices, \eqn{d_t} represents deterministic terms, and \eqn{u_t} denotes
#' the reduced-form innovations.
#'
#' According to Stock and Watson (1988), the VECM can equivalently be written
#' in the common-trend representation
#'
#' \deqn{
#' p_t =
#' p_{t_1} +
#' \Psi(1)\sum_{i=1}^{t}u_i +
#' \Psi^*(L)u_t.
#' }
#'
#' The long-run impact matrix is
#'
#' \deqn{
#' \Psi(1) = C =
#' \beta_\perp
#' \left(
#' \alpha_\perp^\prime
#' \Upsilon
#' \beta_\perp
#' \right)^{-1}
#' \alpha_\perp^\prime,
#' }
#'
#' where
#'
#' \deqn{
#' \Upsilon =
#' I_2 - \sum_{i=1}^{K-1}\Gamma_i,
#' }
#'
#' and \eqn{I_2} is the two-dimensional identity matrix. Under cointegration
#' rank one, \eqn{\beta_\perp} and \eqn{\alpha_\perp} denote vectors
#' orthogonal to \eqn{\beta} and \eqn{\alpha}, respectively. For a
#' two-variable system, they are constructed as
#'
#' \deqn{
#' \beta_\perp =
#' \begin{pmatrix}
#' -\beta_2\\
#' \beta_1
#' \end{pmatrix},
#' \qquad
#' \alpha_\perp =
#' \begin{pmatrix}
#' -\alpha_2\\
#' \alpha_1
#' \end{pmatrix}.
#' }
#'
#' Here, \eqn{p_{t_1}} is a vector of initial values and \eqn{\Psi^*(L)}
#' is a matrix polynomial in the lag operator \eqn{L}. The term
#' \eqn{C\sum_{i=1}^{t}u_i} is the permanent common-trend component, whereas
#' \eqn{\Psi^*(L)u_t} is a stationary transitory component.
#'
#' The scalar efficient price of the asset is therefore
#'
#' \deqn{
#' m_t =
#' \left(
#' \alpha_\perp^\prime
#' \Upsilon
#' \beta_\perp
#' \right)^{-1}
#' \alpha_\perp^\prime
#' \sum_{i=1}^{t}u_i,
#' }
#'
#' whose innovations are serially uncorrelated by construction.
#'
#' The function also reports the correlation between the two residual series
#' stored in the `R0` slot of `vecm_fit`. This correlation is calculated as
#'
#' \deqn{
#' \widehat{\rho} =
#' \operatorname{cor}
#' \left(
#' R_{0,\cdot 1},
#' R_{0,\cdot 2}
#' \right).
#' }
#'
#' A high residual correlation indicates that contemporaneous innovations are
#' difficult to attribute uniquely to either market. This is relevant for the
#' width of the lower and upper Hasbrouck information-share bounds, as discussed
#' by Hasbrouck (1995) and Dimpfl and Schweikert (2023).
#'
#' @section Relation to the referenced methodology:
#'
#' Dimpfl and Schweikert (2023) use the common-trend representation to recover
#' efficient-price innovations during overlapping trading periods. They then
#' calculate realized variance from those innovations, avoiding an arbitrary
#' choice between observed futures and spot returns. Dias and Schweikert (2022)
#' provide supporting evidence for using common-trend returns when the same
#' asset trades in fragmented markets.
#'
#' The paper estimates a high-resolution VECM with HAR-structured lag
#' coefficients. The present function instead receives the standard Johansen
#' VECM fitted by [vecm()] and constructs \eqn{\Upsilon} from its unrestricted
#' short-run coefficient matrices. It therefore implements the same
#' common-trend principle but does not exactly reproduce the paper's HAR-VECM.
#' See [vecm()] for further details on the VECM specification and its relation
#' to the reference paper.
#'
#' @section Input requirements:
#'
#' `vecm_fit` must be a bivariate `ca.jo` object with the futures price ordered
#' first and the spot price second. It should normally be created by [vecm()]
#' so that the coefficient layout is compatible with the package's internal
#' extraction functions.
#'
#' The model must contain at least one differenced lag, meaning that the level
#' VAR lag order used by [vecm()] must be at least two. The relevant residual
#' and coefficient matrices must contain finite values.
#'
#' For the bivariate CWIS framework, `rank` must be 1. This corresponds to two
#' non-stationary price series connected by one cointegrating relationship and
#' driven by one permanent common stochastic trend.
#'
#' The scalar
#'
#' \deqn{
#' \alpha_\perp^\prime\Upsilon\beta_\perp
#' }
#'
#' must be finite and sufficiently different from zero. Otherwise, the
#' long-run impact matrix is singular and the function stops with an error.
#'
#' @param vecm_fit A fitted bivariate `ca.jo` object, normally returned by
#'   [vecm()]. Its variables must be ordered as futures followed by spot.
#' @param rank Integer cointegration rank passed to [urca::cajorls()]. The
#'   bivariate CWIS calculation requires `rank = 1`, corresponding to one
#'   cointegrating relation and one permanent common trend.
#'
#' @return
#' A list with three components:
#'
#' - `common_price`: a numeric vector of length `nrow(vecm_fit@R0)` containing
#'   the first component of the accumulated permanent common trend. The initial
#'   observed price level is not added;
#' - `residual_correlation`: the Pearson correlation between the two residual
#'   columns in `vecm_fit@R0`; a larger absolute correlation is
#'   generally associated with wider Hasbrouck bounds.
#' - `long_run_impact`: a numeric `2` by `2` matrix containing
#'   \eqn{\widehat{C}}, the estimated long-run impact of the residuals on the
#'   permanent price component.
#'
#' @references
#' Stock, J. H. and Watson, M. W. (1988). Testing for common trends.
#' \emph{Journal of the American Statistical Association}, 83(404), 1097--1107.
#' \doi{10.2307/2290142}
#'
#' Gonzalo, J. and Granger, C. (1995). Estimation of common long-memory
#' components in cointegrated systems.
#' \emph{Journal of Business \& Economic Statistics}, 13(1), 27--35.
#'
#' Dias, G. F. and Schweikert, K. (2022). Integrated variance estimation for
#' assets traded in multiple venues. \emph{SSRN Working Paper}, 1--48.
#' \url{https://ssrn.com/abstract=4253762}
#'
#' Dimpfl, T. and Schweikert, K. (2023). Information shares for markets with
#' partially overlapping trading hours. \emph{Journal of Banking & Finance},
#' 154, 106970. \doi{10.1016/j.jbankfin.2023.106970}
#' @seealso
#' [vecm()] for fitting the bivariate Johansen VECM; [urca::cajorls()] for
#' estimating the restricted VECM at a specified cointegration rank; [his()]
#' for Hasbrouck information shares; [realized_variance()] for estimating
#' variation from the common-price changes; [daily_cwis()] for the complete
#' daily CWIS calculation.
#'
#' @examples
#' set.seed(123)
#'
#' n <- 300
#' common_trend <- cumsum(rnorm(n, sd = 0.002))
#' stationary_spread <- as.numeric(
#'   stats::arima.sim(model = list(ar = 0.7), n = n, sd = 0.0005)
#' )
#'
#' simulated_log_prices <- cbind(
#'   futures = log(5000) + common_trend + stationary_spread / 2,
#'   spot = log(5000) + common_trend - stationary_spread / 2
#' )
#'
#' fitted_vecm <- vecm(
#'   log_prices = simulated_log_prices,
#'   K = 2,
#'   type = "trace",
#'   ecdet = "none",
#'   spec = "transitory"
#' )
#'
#' common <- common_price(fitted_vecm, rank = 1L)
#'
#' names(common)
#' head(common$common_price)
#' common$residual_correlation
#' common$long_run_impact
#'
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

