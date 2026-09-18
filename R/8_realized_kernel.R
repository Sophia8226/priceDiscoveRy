#' Estimate realized-kernel variance for a single-market session
#'
#' @description
#' Estimates the integrated variance of one price series during a trading
#' session in which only one market is open. A generalized realized-kernel
#' estimator is used to reduce the effect of market-microstructure noise in
#' high-frequency returns.
#'
#' @details
#' During a non-overlapping trading period, only one market price is observed.
#' Following Wang and Yang (2011), the open market is assigned the price
#' discovery occurring during that period. Its contribution to the daily CWIS
#' is measured by the variation in the efficient price generated while that
#' market is open.
#'
#' At very high sampling frequencies, observed returns may be serially
#' correlated because of bid-ask bounce, price discreteness, and other
#' microstructure effects. The ordinary realized-variance estimator may
#' therefore be distorted. Dimpfl and Schweikert (2023) address this problem
#' in non-overlapping sessions using the realized-kernel estimator developed
#' by Barndorff-Nielsen et al. (2008).
#'
#' For a session containing \eqn{n} intraday returns, the estimator is
#'
#' \deqn{
#' \widehat{IV}_{K}^{(n)}
#' =
#' \sum_{i=1}^{n} r_{i,n}^{2}
#' +
#' \sum_{b=1}^{B}
#' k\left(\frac{b-1}{B}\right)
#' \left(\eta_b+\eta_{-b}\right),
#' }
#'
#' where
#'
#' \deqn{
#' \eta_b
#' =
#' \sum_{i=1}^{n-b} r_{i,n}r_{i+b,n}.
#' }
#'
#' Here, \eqn{r_{i,n}} is the \eqn{i}-th intraday return, \eqn{n} is the
#' number of returns, \eqn{B} is the kernel bandwidth,
#' \eqn{k(\cdot)} is the selected kernel-weight function, and
#' \eqn{\eta_b} and \eqn{\eta_{-b}} are return cross-products at positive and
#' negative lag \eqn{b}. The weighted autocovariance terms correct for serial
#' dependence induced by market-microstructure noise.
#'
#' The selected price series is converted to an [xts::xts()] object.
#' [highfrequency::rKernelCov()] optionally aligns the prices to the requested
#' sampling frequency, constructs returns internally, and applies the selected
#' kernel. Only the first univariate variance estimate is returned.
#'
#' The resulting estimate is subsequently used as the variance contribution
#' of the corresponding single-market session when computing the period
#' weights and the daily contribution-weighted information share.
#'
#' @section Sampling and bandwidth:
#'
#' `align_by` and `align_period` determine the sampling frequency before
#' returns are calculated. For example, `align_by = "seconds"` and
#' `align_period = 1` produce one-second sampling, whereas
#' `align_by = "minutes"` and `align_period = 5` produce five-minute sampling.
#'
#' Supported time units are `"ticks"`, `"secs"`, `"seconds"`, `"mins"`,
#' `"minutes"`, and `"hours"`. The underlying function does not provide a
#' `"milliseconds"` option. To retain observations that have already been
#' sampled at a sub-second frequency, such as 10 milliseconds, set
#' `align_by = NULL`; `align_period` is then ignored.
#'
#' Let \eqn{N} denote the number of rows in `session_data`. This wrapper selects
#' the kernel parameter as
#'
#' \deqn{
#' B = \left\lfloor c\sqrt{N}\right\rfloor,
#' }
#'
#' where \eqn{c} is `bandwidth_constant`. If `bandwidth_constant = NULL`,
#' \eqn{c=2.3970} is used for the modified Tukey-Hanning kernel and
#' \eqn{c=3.5134} for the Parzen kernel. For any other kernel,
#' `bandwidth_constant` must be supplied explicitly.
#'
#' Dimpfl and Schweikert (2023) use the modified Tukey-Hanning kernel with
#' the bandwidth selected according to the rule-of-thumb proposed by
#' Barndorff-Nielsen et al. (2009).
#'
#' @section Input requirements:
#'
#' `session_data` must be a data frame containing `datetime` and the column
#' selected by `price_col`. It should represent one standardized single-market
#' session, normally obtained from [split_trading_sessions()].
#'
#' The `datetime` column must contain chronologically ordered timestamps
#' suitable for constructing an `xts` index. Timestamps should be unique and
#' belong to one trading day and one trading session.
#'
#' The selected price column must contain at least two numeric observations.
#' All selected prices must be finite and strictly positive; missing values
#' are not permitted. More observations may be required for a meaningful
#' kernel estimate, particularly when a large bandwidth or coarse sampling
#' interval is selected.
#'
#' @param session_data A standardized data frame for one single-market
#'   trading session. It must contain a `datetime` column and the price series
#'   selected by `price_col`.
#' @param price_col Character string identifying the price series to use.
#'   Must be either `"futures"` or `"spot"`.
#' @param kernel_type Character string specifying the kernel used by
#'   [highfrequency::rKernelCov()] to estimate realized kernels for
#'   non-overlapping trading sessions. Available kernel names can be inspected
#'   with [highfrequency::listAvailableKernels()]. The default,
#'   `"ModifiedTukeyHanning"`, follows Dimpfl and Schweikert (2023).
#' @param bandwidth_constant Optional positive numeric multiplier \eqn{c} used
#'   to set the session-specific kernel parameter passed to
#'   [highfrequency::rKernelCov()] as
#'   \eqn{B=\lfloor c\sqrt{N}\rfloor}, where \eqn{N} is the number of rows in
#'   the session data. When `NULL`, `c = 2.3970` is used for
#'   `"ModifiedTukeyHanning"` and `c = 3.5134` for `"Parzen"`. Other kernel
#'   types require an explicitly supplied positive value.
#' @param align_by Character string specifying the sampling unit in which
#'   `align_period` is expressed when aligning the non-overlapping session
#'   data. Supported values include `"ticks"`, `"seconds"`, `"minutes"`, and
#'   `"hours"`. Set to `NULL` to retain the original observation frequency,
#'   in which case `align_period` is ignored.
#' @param align_period Positive numeric value specifying the number of
#'   `align_by` units in each sampling interval. It is ignored when
#'   `align_by = NULL`.
#'
#' @return
#' An unnamed numeric scalar containing the realized-kernel estimate of
#' integrated variance for the selected price series and session. The result
#' is expressed in squared-return units; it is neither annualized nor converted
#' to volatility by taking its square root.
#'
#' @references
#' Hansen, P. R. and Lunde, A. (2006). Realized variance and market
#' microstructure noise. \emph{Journal of Business \& Economic Statistics},
#' 24(2), 127--161. \doi{10.1198/073500106000000071}
#'
#' Barndorff-Nielsen, O. E., Hansen, P. R., Lunde, A., and Shephard, N. (2008).
#' Designing realized kernels to measure the ex post variation of equity prices
#' in the presence of noise. \emph{Econometrica}, 76(6), 1481--1536. \doi{10.3982/ECTA6495}
#'
#' Barndorff-Nielsen, O. E., Hansen, P. R., Lunde, A., and Shephard, N. (2009).
#' Realized kernels in practice: Trades and quotes.
#' \emph{The Econometrics Journal}, 12(3), 1--32. \doi{10.1111/j.1368-423X.2008.00275.x}
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
#' [highfrequency::rKernelCov()] for the underlying estimator;
#' [highfrequency::listAvailableKernels()] for the supported kernels;
#' [realized_variance()] for ordinary realized variance;
#' [split_trading_sessions()] for constructing session data;
#' [combine_weights()] for converting session variances into contribution
#' weights; and [daily_cwis()] for the complete daily calculation.
#'
#' @examples
#' set.seed(123)
#'
#' n <- 600L
#' session <- data.frame(
#'   datetime = as.POSIXct(
#'     "2026-01-05 00:00:00",
#'     tz = "America/New_York"
#'   ) + seq_len(n) - 1,
#'   trading_day = as.Date("2026-01-05"),
#'   futures = 5000 * exp(cumsum(rnorm(n, sd = 0.0001))),
#'   spot = NA_real_
#' )
#'
#' rk <- realized_kernel(
#'   session_data = session,
#'   price_col = "futures",
#'   kernel_type = "ModifiedTukeyHanning",
#'   align_by = "seconds",
#'   align_period = 1
#' )
#'
#' rk
#'
#' @export
realized_kernel <- function(
    session_data,
    price_col,
    kernel_type = "ModifiedTukeyHanning",
    bandwidth_constant = NULL,
    align_by = "seconds",
    align_period = 1L) {
  if (!is.character(price_col) || length(price_col) != 1L ||
      !price_col %in% c("futures", "spot")) {
    stop("`price_col` must be either \"futures\" or \"spot\".", call. = FALSE)
  }
  if (!is.data.frame(session_data) ||
      !all(c("datetime", price_col) %in% names(session_data))) {
    stop("`session_data` does not contain the requested price series.", call. = FALSE)
  }
  if (nrow(session_data) < 2L) {
    stop("A single-market session needs at least two observations.", call. = FALSE)
  }

  price <- session_data[[price_col]]
  if (any(!is.finite(price)) || any(price <= 0)) {
    stop("Kernel prices must be finite and strictly positive.", call. = FALSE)
  }

  if (is.null(bandwidth_constant)) {
    bandwidth_constant <- switch(
      kernel_type,
      ModifiedTukeyHanning = 2.3970,
      Parzen = 3.5134,
      stop(
        "Supply `bandwidth_constant` for kernel type ", kernel_type, ".",
        call. = FALSE
      )
    )
  }
  kernel_parameter <- as.integer(bandwidth_constant * sqrt(nrow(session_data)))
  price_xts <- xts::xts(price, order.by = session_data$datetime)

  result <- highfrequency::rKernelCov(
    rData = price_xts,
    align.by = align_by,
    align.period = align_period,
    makeReturns = TRUE,
    kernel.type = kernel_type,
    kernel.param = kernel_parameter
  )
  unname(as.numeric(result)[[1L]])
}
