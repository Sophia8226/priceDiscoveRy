#' Combine period variances into contribution weights
#'
#' @description
#' Combines integrated-variance estimates from overlapping and
#' non-overlapping trading sessions and converts them into normalized
#' contribution weights for the daily CWIS calculation.
#'
#' @details
#' Consider a trading day divided into \eqn{H} mutually exclusive periods
#' \eqn{S_h}, for \eqn{h = 1, \ldots, H}. Some periods contain simultaneous
#' trading in both markets, whereas others contain trading in only one market.
#'
#' Following Wang and Yang (2011), information generated during a period is
#' measured by the variation in the efficient price during that period.
#' Dimpfl and Schweikert (2023) extend this variance-share principle to
#' partially overlapping markets.
#'
#' Let \eqn{\widehat{\sigma}_{m,h}^{2}} denote the estimated integrated
#' variance of the efficient price \eqn{m} during period \eqn{S_h}. The
#' estimated total daily variance is
#'
#' \deqn{
#' \widehat{\sigma}_{m,d}^{2}
#' =
#' \sum_{i=1}^{H}\widehat{\sigma}_{m,i}^{2}.
#' }
#'
#' The contribution weight of period \eqn{S_h} is then
#'
#' \deqn{
#' w_h
#' =
#' \frac{\widehat{\sigma}_{m,h}^{2}}
#' {\sum_{i=1}^{H}\widehat{\sigma}_{m,i}^{2}}.
#' }
#'
#' Here, \eqn{w_h} is the proportion of total daily efficient-price variation
#' generated during period \eqn{S_h}. Provided that all period variances are
#' finite and non-negative and their sum is positive,
#'
#' \deqn{
#' 0 \leq w_h \leq 1
#' \quad\mbox{and}\quad
#' \sum_{h=1}^{H} w_h = 1.
#' }
#'
#' For overlapping sessions, \eqn{\widehat{\sigma}_{m,h}^{2}} is normally
#' calculated from returns of the common efficient price extracted from the
#' VECM. This follows the common-trend approach discussed by Dias and
#' Schweikert (2022) and Dimpfl and Schweikert (2023).
#'
#' For non-overlapping sessions, only one market price is observed. The
#' corresponding period variance is normally estimated with a realized
#' kernel, following Wang and Yang (2011), Barndorff-Nielsen et al. (2008),
#' and Dimpfl and Schweikert (2023).
#'
#' The total overlapping weight is the sum of the weights assigned to all
#' subperiods in which both markets are open:
#'
#' \deqn{
#' w_{\mathrm{overlap}}
#' =
#' w_{\mathrm{overlap\_pre}}
#' +
#' w_{\mathrm{overlap\_core}}
#' +
#' w_{\mathrm{overlap\_post}}.
#' }
#'
#' The weights measure contributions to daily efficient-price variation, not
#' proportions of trading time. Consequently, a short but volatile session
#' may receive a relatively large weight.
#'
#' @section Interpretation:
#'
#' The weights measure contributions to price variation, not proportions of
#' trading time. A short but volatile period may therefore receive a larger
#' weight than a longer period with little efficient-price variation.
#'
#' In the final CWIS calculation, the overlapping-period weight is multiplied
#' by the Hasbrouck information shares estimated while both markets are open.
#' A non-overlapping-period weight is assigned entirely to the market that is
#' open during that period.
#'
#' The calculation assumes that all supplied values estimate the same
#' variance concept and are expressed on a comparable scale. It also relies
#' on the additivity of quadratic variation across disjoint intraday periods.
#'
#' As noted by Dimpfl and Schweikert (2023), these weights do not adjust for
#' possible differences in estimation precision, sampling error, or
#' microstructure noise across periods. Their advantage is that they retain
#' a direct interpretation as shares of total efficient-price variation.
#' Hansen and Lunde (2005) discuss alternative approaches for combining
#' variance estimates obtained from different portions of a trading day.
#'
#' @section Input requirements:
#'
#' `overlapping_variance` and `non_overlapping_variance` must be named numeric
#' vectors. Each element must contain one integrated-variance estimate for
#' one trading period.
#'
#' All names must be non-missing, non-empty, and unique across both vectors.
#' The names are retained in the returned vectors and are used to determine
#' which weights belong to overlapping periods.
#'
#' All variance estimates must be finite and non-negative. Their combined sum
#' must be strictly positive. The function does not verify how the estimates
#' were obtained or whether they belong to the same trading day.
#'
#' @param overlapping_variance A named numeric vector containing integrated-
#'   variance estimates for periods in which both markets trade. These values
#'   are normally estimated from common-efficient-price returns using
#'   [realized_variance()].
#' @param non_overlapping_variance A named numeric vector containing
#'   integrated-variance estimates for periods in which only one market
#'   trades. These values are normally estimated using [realized_kernel()].
#'
#' @return
#' A list with three components:
#'
#' \describe{
#'   \item{`realized_variance`}{
#'     A named numeric vector containing all supplied period variances.
#'     Non-overlapping periods appear first, followed by overlapping periods.
#'   }
#'   \item{`variance_weights`}{
#'     A named numeric vector containing the normalized period weights
#'     \eqn{w_h}. It has the same names and order as `realized_variance` and
#'     sums to one, up to numerical precision.
#'   }
#'   \item{`overlapping_weight`}{
#'     A numeric scalar equal to the sum of the weights belonging to
#'     `overlapping_variance`. It lies between zero and one.
#'   }
#' }
#'
#' @references
#' Hansen, P. R. and Lunde, A. (2005). A realized variance for the whole day
#' based on intermittent high-frequency data.
#' \emph{Journal of Financial Econometrics}, 3(4), 525--554. \doi{10.1093/jjfinec/nbi028}
#'
#' Wang, J. and Yang, M. (2011). Housewives of Tokyo versus the gnomes of
#' Zurich: Measuring price discovery in sequential markets.
#' \emph{Journal of Financial Markets}, 14(1), 82--108. \doi{10.1016/j.finmar.2010.08.002}
#'
#' Dimpfl, T. and Schweikert, K. (2023). Information shares for markets with
#' partially overlapping trading hours. \emph{Journal of Banking & Finance},
#' 154, 106970. \doi{10.1016/j.jbankfin.2023.106970}
#'
#' @seealso
#' [common_price()] for extracting the common efficient price;
#' [realized_variance()] for overlapping-period variance;
#' [realized_kernel()] for single-market-period variance;
#' [his()] for Hasbrouck information shares;
#' [aggregate_cwis()] for combining the variance weights with information shares; and
#' [daily_cwis()] for the complete daily estimation procedure.
#'
#' @examples
#' overlapping_variance <- c(
#'   overlap_pre = 1.2e-5,
#'   overlap_core = 4.5e-5,
#'   overlap_post_1 = 0.5e-5,
#'   overlap_post_2 = 0.8e-5
#' )
#'
#' non_overlapping_variance <- c(
#'   futures_pre = 1.0e-5,
#'   spot_maintenance = 0.5e-5,
#'   futures_post = 1.5e-5
#' )
#'
#' result <- combine_weights(
#'   overlapping_variance = overlapping_variance,
#'   non_overlapping_variance = non_overlapping_variance
#' )
#'
#' result$realized_variance
#' result$variance_weights
#' result$overlapping_weight
#'
#' sum(result$variance_weights)
#'
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
