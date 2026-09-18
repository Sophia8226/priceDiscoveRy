#' Compute Hasbrouck information shares for one VECM ordering
#'
#' @description
#' Computes Hasbrouck information shares for futures and spot markets from a
#' fitted bivariate VECM and a specified ordering of the two price series.
#'
#' @details
#' When two markets trade the same underlying asset, their observed prices may
#' differ temporarily but are expected to share one permanent common stochastic
#' trend. Hasbrouck's (1995) information share measures the proportion of the
#' innovation variance of this common trend attributable to each market.
#'
#' These common long-run and market-specific short-run dynamics can be
#' represented by a cointegrated vector error-correction model (VECM). For the
#' default transitory specification, the model estimated by [urca::ca.jo()] can
#' be summarized as
#'
#' \deqn{
#' \Delta p_t =
#' \Pi p_{t-1} +
#' \sum_{i=1}^{K-1}\Gamma_i\Delta p_{t-i} +
#' d_t + u_t,
#' }
#'
#' where \eqn{p_t} contains the futures and spot log prices,
#' \eqn{\Pi=\alpha\beta^\prime} is the long-run coefficient matrix,
#' \eqn{\beta} is the cointegrating vector, \eqn{\alpha} contains the
#' adjustment coefficients, \eqn{\Gamma_i} contains the short-run coefficient
#' matrices, \eqn{d_t} represents the deterministic terms, and \eqn{u_t} is a
#' vector of reduced-form white-noise innovations satisfying
#' \eqn{E(u_t)=0} and
#'
#' \deqn{
#' \Omega =
#' E\left(u_tu_t^\prime\right)
#' =
#' \operatorname{Var}(u_t).
#' }
#'
#' To allocate the contemporaneous covariance among the markets, the innovation
#' covariance matrix is factorized as
#'
#' \deqn{
#' \Omega = FF^\prime,
#' }
#'
#' where \eqn{F} is a lower-triangular Cholesky factor. In the implementation,
#' this factor is calculated from the covariance matrix stored in the `DELTA`
#' slot of `vecm_fit` using
#' `F <- t(chol(vecm_fit@DELTA))`.
#'
#' Assuming that the cointegrated system is driven by one common stochastic
#' trend, let \eqn{\psi} denote a row of the long-run impact matrix
#' \eqn{\Psi(1)}. The variance of the common-trend innovation is then
#' \eqn{\psi\Omega\psi^\prime}.
#'
#' The Hasbrouck information share of market \eqn{j} is
#'
#' \deqn{
#' S_j =
#' \frac{
#' [\psi F]_j^2
#' }{
#' \psi\Omega\psi^\prime
#' },
#' \qquad j=1,2.
#' }
#'
#' The numerator is the squared contribution of the orthogonalized innovation
#' associated with market \eqn{j}. The denominator is the total innovation
#' variance of the permanent common-price component. Thus, \eqn{S_j} measures
#' the proportion of permanent price innovation attributed to market
#' \eqn{j} under the selected ordering.
#'
#' @section Ordering and information-share bounds:
#'
#' When the market innovations are contemporaneously correlated, their joint
#' covariance cannot be allocated uniquely to an individual market. The
#' Cholesky decomposition resolves this covariance sequentially and therefore
#' depends on the ordering of the markets.
#'
#' Following Hasbrouck (1995), placing a market first in the ordering maximizes
#' the information share assigned to that market and provides its upper bound.
#' Placing the same market last minimizes its assigned information share and
#' provides its lower bound. For two markets, both possible orderings are
#' therefore evaluated, giving
#'
#' \deqn{
#' S_j^L \leq S_j \leq S_j^U.
#' }
#'
#' The bounds may coincide when the innovations are uncorrelated. A wider
#' interval between \eqn{S_j^L} and \eqn{S_j^U} indicates greater uncertainty
#' about how the contemporaneous covariance should be assigned.
#'
#' The midpoint of the lower and upper bounds is commonly reported as a single
#' information-share estimate:
#'
#' \deqn{
#' S_j^* =
#' \frac{
#' S_j^L + S_j^U
#' }{
#' 2
#' }.
#' }
#'
#' This function calculates information shares for only the ordering stored in
#' `vecm_fit`. The reversed ordering must be estimated separately to obtain the
#' other bound. [daily_cwis()] performs both ordering-specific calculations and
#' uses \eqn{S_j^*} as the overlapping-period HIS estimate, following Dimpfl
#' and Schweikert (2023).
#'
#' High contemporaneous innovation correlation can produce wide bounds and
#' make their midpoint less informative. Using a sufficiently high sampling
#' frequency may reduce this correlation, although the sampling frequency
#' should also be selected with regard to market-microstructure noise.
#'
#' @section Relation to the referenced methodology:
#'
#' The information-share decomposition follows Hasbrouck (1995). Dimpfl and
#' Schweikert (2023) apply it to the periods in which futures and ETF markets
#' trade simultaneously and use the midpoint of the two ordering-based bounds
#' in their contribution-weighted information share.
#'
#' The paper obtains the required long-run quantities from a high-resolution
#' HAR-VECM following Hasbrouck (2021). This function instead extracts them
#' from the standard Johansen VECM returned by [vecm()]. It therefore applies
#' the Hasbrouck decomposition to the package's VECM implementation but does
#' not reproduce the paper's HAR-VECM exactly. See [vecm()] for further details
#' on the VECM specification and its relation to the reference paper.
#'
#' @section Input requirements:
#'
#' `vecm_fit` must be a fitted bivariate `ca.jo` object whose first variable is
#' the futures log price and whose second variable is the spot log price. It
#' should normally be returned by [vecm()].
#'
#' The model must be suitable for a rank-one representation and contain at
#' least one differenced lag. The package's downstream methodology is designed
#' for the default transitory VECM specification.
#'
#' The covariance matrix stored in `vecm_fit@DELTA` must be finite,
#' symmetric, and positive definite so that its Cholesky decomposition exists.
#' The scalar
#'
#' \deqn{
#' \alpha_\perp^\prime\Upsilon\beta_\perp
#' }
#'
#' must be finite and sufficiently different from zero. The total common-trend
#' innovation variance must also be strictly positive. Otherwise, the function
#' stops with an error.
#'
#' @param vecm_fit A fitted bivariate `ca.jo` object, normally returned by
#'   [vecm()]. Its variables must be ordered as futures followed by spot.
#'
#' @return
#' A named numeric vector of length two with components:
#'
#' - `futures`: the Hasbrouck information share assigned to the first variable
#'   under the current Cholesky ordering;
#' - `spot`: the Hasbrouck information share assigned to the second variable
#'   under the current Cholesky ordering.
#'
#' When the decomposition is well-defined, both values are non-negative and
#' sum to one. The returned values represent one ordering-specific allocation;
#' they are not, by themselves, complete lower and upper bounds.
#'
#' @references
#' Stock, J. H. and Watson, M. W. (1988). Testing for common trends.
#' \emph{Journal of the American Statistical Association}, 83(404), 1097--1107.
#' \doi{10.2307/2290142}
#'
#' Hasbrouck, J. (1995). One security, many markets: Determining the
#' contributions to price discovery.\emph{The Journal of Finance}, 50(4),
#' 1175--1199.\doi{10.2307/2329348}
#'
#' Putniņš, T. J. (2013). What do price discovery metrics really measure?
#' \emph{Journal of Empirical Finance}, 23, 68--83.
#' \doi{10.1016/j.jempfin.2013.05.004}
#'
#' Hasbrouck, J. (2021). Price discovery in high resolution.
#' \emph{Journal of Financial Econometrics}, 19(3), 395--430.
#' \doi{10.1093/jjfinec/nbz027}
#'
#' Dimpfl, T. and Schweikert, K. (2023). Information shares for markets with
#' partially overlapping trading hours. \emph{Journal of Banking & Finance},
#' 154, 106970. \doi{10.1016/j.jbankfin.2023.106970}
#'
#' @seealso
#' [vecm()] for estimating the bivariate VECM; [common_price()] for extracting
#' the permanent common-price component; [daily_cwis()] for computing both
#' Cholesky orderings, the information-share bounds, and their midpoint;
#' [combine_weights()] for combining the midpoint with period-specific
#' variance weights.
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
#' log_prices <- cbind(
#'   futures = log(5000) + common_trend + stationary_spread / 2,
#'   spot = log(5000) + common_trend - stationary_spread / 2
#' )
#'
#' # Information shares for the futures-first ordering
#' fit_futures_first <- vecm(
#'   log_prices = log_prices,
#'   K = 2,
#'   type = "trace",
#'   ecdet = "none",
#'   spec = "transitory"
#' )
#'
#' shares_futures_first <- his(fit_futures_first)
#' shares_futures_first
#' sum(shares_futures_first)
#'
#' # Repeat the calculation after reversing the market order
#' fit_spot_first <- vecm(
#'   log_prices = log_prices[, c("spot", "futures")],
#'   K = 2,
#'   type = "trace",
#'   ecdet = "none",
#'   spec = "transitory"
#' )
#'
#' shares_spot_first_position <- his(fit_spot_first)
#'
#' # Restore the reversed result to the original futures-spot order
#' shares_spot_first <- stats::setNames(
#'   rev(unname(shares_spot_first_position)),
#'   c("futures", "spot")
#' )
#'
#' bounds <- cbind(
#'   lower = pmin(shares_futures_first, shares_spot_first),
#'   upper = pmax(shares_futures_first, shares_spot_first)
#' )
#'
#' midpoint <- rowMeans(bounds)
#'
#' bounds
#' midpoint
#'
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
