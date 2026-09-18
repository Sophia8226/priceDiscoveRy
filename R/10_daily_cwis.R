.slice_returns <- function(returns, lengths) {
  endpoints <- cumsum(lengths)
  starts <- c(1L, endpoints[-length(endpoints)] + 1L)
  parts <- Map(function(start, end) returns[start:end], starts, endpoints)
  stats::setNames(parts, names(lengths))
}

#' Calculate CWIS for one trading day
#'
#' @description
#' Calculates the contribution-weighted information share (CWIS) for a
#' futures-spot pair over one trading day with partially overlapping trading
#' hours.
#'
#' The function combines Hasbrouck information shares from overlapping trading
#' periods with realized variances from non-overlapping periods.
#'
#' @details
#'
#' `day_data` is assumed to have been validated and standardized by
#' [validate_and_standardize_input()] and restricted to a single trading day
#' by [split_trading_days()]. In the complete package workflow, these
#' preprocessing steps are performed by [cwis()]. The daily calculation then
#' proceeds as follows:
#'
#' \enumerate{
#'   \item [split_trading_sessions()] divides the observations into overlapping
#'   and single-market sessions. See that function for the exact session
#'   definitions.
#'
#'   \item The overlapping observations are pooled, and [vecm()] estimates the
#'   bivariate VECM under both possible market orderings.
#'
#'   \item [his()] is applied to both fitted VECMs. The two ordering-specific
#'   estimates provide the lower and upper Hasbrouck bounds for each market,
#'   and their average is used as the overlapping-period midpoint estimate.
#'
#'   \item [common_price()] extracts the common efficient price. Its returns
#'   are assigned to the pre-core, core, and combined post-core overlapping
#'   subperiods, for which [realized_variance()] estimates price variation.
#'
#'   \item [realized_kernel()] estimates price variation during each
#'   single-market session.
#'
#'   \item [combine_weights()] converts the session-specific variance estimates
#'   into normalized weights and calculates the total overlapping weight.
#'
#'   \item Finally, following Dimpfl and Schweikert (2023), the function
#'   calculates the daily CWIS for both markets:
#'
#'   \deqn{
#'   \begin{pmatrix}
#'   S_{cw,1}^{*}\\
#'   S_{cw,2}^{*}
#'   \end{pmatrix}
#'   =
#'   w_{\mathrm{overlap}}
#'   \begin{pmatrix}
#'   S_1^{*}\\
#'   S_2^{*}
#'   \end{pmatrix}
#'   +
#'   w_{\mathrm{market1}}
#'   \begin{pmatrix}
#'   1\\
#'   0
#'   \end{pmatrix}
#'   +
#'   w_{\mathrm{market2}}
#'   \begin{pmatrix}
#'   0\\
#'   1
#'   \end{pmatrix}.
#'   }
#'
#'   Here, \eqn{S_j^{*}} is the midpoint Hasbrouck information share of
#'   market \eqn{j} during overlapping trading. The term
#'   \eqn{w_{\mathrm{overlap}}} is the total weight of the overlapping
#'   subperiods. The terms \eqn{w_{\mathrm{market1}}} and
#'   \eqn{w_{\mathrm{market2}}} are the total weights of the periods in which
#'   only market 1 or only market 2 is open.
#'
#'   In this package, market 1 is the futures market and market 2 is the spot
#'   market. Therefore, \eqn{w_{\mathrm{market1}}} combines the weights of
#'   `futures_pre` and `futures_post`, while
#'   \eqn{w_{\mathrm{market2}}} is the weight of `spot_maintenance`.
#'
#'   When the weights are normalized, the two daily CWIS estimates sum to one.
#'   If \eqn{w_{\mathrm{overlap}}=1}, they equal the midpoint Hasbrouck
#'   information shares.
#' }
#'
#' Each referenced function documents the corresponding calculation and
#' input requirements in detail.
#'
#' @section Relationship to the reference paper:
#'
#' Dimpfl and Schweikert (2023), following Hasbrouck (2021) and Buccheri et
#' al. (2021), use a high-resolution VECM with HAR-structured lag coefficients.
#' At a 10-millisecond sampling frequency, their specification uses four
#' HAR time-scale blocks to cover lagged price changes over a maximum horizon
#' of ten seconds.
#'
#' The present function instead calls [vecm()], which estimates the standard
#' Johansen VECM implemented by [urca::ca.jo()]. Therefore, this function
#' implements the CWIS framework and its variance-weighting principle but is
#' not an exact replication of the paper's high-resolution HAR-VECM. See
#' [vecm()] for further details on the VECM specification and its relation to
#' the reference paper.
#'
#' @section Input requirements:
#'
#' `day_data` must be a standardized data frame containing the columns
#' `datetime`, `trading_day`, `futures`, and `spot`. It should normally be
#' obtained from [validate_and_standardize_input()] and must contain exactly
#' one trading day.
#'
#' Timestamps must be chronologically ordered and must be interpretable in the
#' time zone stored in the `"tz"` attribute. Session membership is determined
#' from local clock time using `session_boundaries`.
#'
#' Each of the seven required component sessions must contain at least two
#' observations. Both price series must be available, finite, and strictly
#' positive during overlapping sessions. The price of the open market must be
#' available, finite, and strictly positive during each non-overlapping
#' session.
#'
#' The pooled overlapping sample must contain more than `K + 2` complete
#' observations, and both overlapping price series must vary. Missing values
#' are not imputed.
#'
#' @param day_data A standardized data frame containing exactly one trading
#'   day and the columns `datetime`, `trading_day`, `futures`, and `spot`.
#'
#' @inheritParams vecm K
#'
#' @inheritParams realized_kernel kernel_type bandwidth_constant align_by align_period
#'
#' @param session_boundaries A strictly increasing named numeric vector defining
#'   the trading-session boundaries in seconds after local midnight. It must
#'   contain, in order, `overlap_open`, `core_open`,
#'   `core_close_exclusive`, `maintenance_start`, `overlap_resume`, and
#'   `overlap_close`. All values must be finite and lie strictly between
#'   00:00:00 and 24:00:00. See [split_trading_sessions()] for the precise
#'   interval definitions.
#'
#' @return
#' An object of class `cwis_day_result`, implemented as a list with the
#' following components:
#'
#' \describe{
#'   \item{`date`}{
#'     The single trading date represented by `day_data`.
#'   }
#'   \item{`his_lower`}{
#'     A named numeric vector containing the lower Hasbrouck information-share
#'     bound for futures and spot.
#'   }
#'   \item{`his_upper`}{
#'     A named numeric vector containing the upper Hasbrouck information-share
#'     bound for futures and spot.
#'   }
#'   \item{`his_midpoint`}{
#'     A named numeric vector containing the midpoint of the lower and upper
#'     Hasbrouck bounds.
#'   }
#'   \item{`residual_correlation`}{
#'     The Pearson correlation between the two residual columns in `vecm_fit@R0`;
#'     A high correlation is generally associated with wider
#'     Hasbrouck bounds.
#'   }
#'   \item{`zero_return_fraction`}{
#'     A named numeric vector giving the proportion of zero log returns in the
#'     pooled overlapping sample for futures and spot.
#'   }
#'   \item{`realized_variance`}{
#'     A named numeric vector containing variance estimates for
#'     `futures_pre`, `spot_maintenance`, `futures_post`, `overlap_pre`,
#'     `overlap_core`, and `overlap_post`.
#'   }
#'   \item{`variance_weights`}{
#'     The corresponding normalized period weights. These sum to one up to
#'     numerical precision.
#'   }
#'   \item{`overlapping_weight`}{
#'     The total weight assigned to `overlap_pre`, `overlap_core`, and
#'     `overlap_post`.
#'   }
#'   \item{`cwis`}{
#'     A named numeric vector containing the daily CWIS for `futures` and
#'     `spot`.
#'   }
#'   \item{`n_observations`}{
#'     The total number of rows in `day_data`.
#'   }
#'   \item{`n_overlap`}{
#'     The total number of observations in the four overlapping component
#'     sessions.
#'   }
#' }
#'
#' @references
#' Hasbrouck, J. (2021). Price discovery in high resolution.
#' \emph{Journal of Financial Econometrics}, 19(3), 395--430.
#' \doi{10.1093/jjfinec/nbz027}
#'
#' Buccheri, G., Bormetti, G., Corsi, F., and Lillo, F. (2021). Comment on:
#' Price discovery in high resolution. \emph{Journal of Financial Econometrics},
#' 19(3), 439--451. \doi{10.1093/jjfinec/nbz008}
#'
#' Dimpfl, T. and Schweikert, K. (2023). Information shares for markets with
#' partially overlapping trading hours. \emph{Journal of Banking & Finance},
#' 154, 106970. \doi{10.1016/j.jbankfin.2023.106970}
#'
#' @seealso
#' [split_trading_sessions()] for constructing the trading sessions;
#' [vecm()] for estimating the overlapping-period VECM;
#' [his()] for Hasbrouck information-share bounds;
#' [common_price()] for extracting the common efficient price;
#' [realized_variance()] for overlapping-period variance;
#' [realized_kernel()] for single-market variance;
#' [combine_weights()] for variance weights; and
#' [aggregate_cwis()] for aggregating results across trading days.
#'
#' @examples
#' set.seed(123)
#'
#' n <- 420L
#' datetime <- as.POSIXct(
#'   "2026-01-05 00:00:00",
#'   tz = "America/New_York"
#' ) + 0:(n - 1L)
#'
#' common_trend <- cumsum(rnorm(n, sd = 0.0002))
#' futures <- 5000 * exp(common_trend + rnorm(n, sd = 0.00005))
#' spot <- 5000 * exp(common_trend + rnorm(n, sd = 0.00007))
#'
#' # Futures are closed during the spot-only session.
#' futures[241:300] <- NA_real_
#'
#' # Spot is closed during the futures-only sessions.
#' spot[c(1:60, 361:420)] <- NA_real_
#'
#' day_data <- data.frame(
#'   datetime = datetime,
#'   trading_day = as.Date("2026-01-05"),
#'   futures = futures,
#'   spot = spot
#' )
#' attr(day_data, "tz") <- "America/New_York"
#'
#' example_boundaries <- c(
#'   overlap_open = 60,
#'   core_open = 120,
#'   core_close_exclusive = 180,
#'   maintenance_start = 240,
#'   overlap_resume = 300,
#'   overlap_close = 360
#' )
#'
#' result <- daily_cwis(
#'   day_data = day_data,
#'   K = 2L,
#'   session_boundaries = example_boundaries
#' )
#'
#' result$cwis
#' result$his_midpoint
#' result$variance_weights
#' result$overlapping_weight
#'
#' @export
daily_cwis <- function(
    day_data,
    K = 10L,
    kernel_type = "ModifiedTukeyHanning",
    bandwidth_constant = NULL,
    align_by = "seconds",
    align_period = 1L,
    session_boundaries = .default_session_boundaries()) {
  sessions <- split_trading_sessions(
    day_data,
    boundaries = session_boundaries
  )

  required_sessions <- c(
    "futures_pre", "overlap_pre", "overlap_core", "overlap_post_1",
    "spot_maintenance", "overlap_post_2", "futures_post"
  )
  empty_sessions <- required_sessions[vapply(
    sessions[required_sessions], nrow, integer(1L)
  ) < 2L]
  if (length(empty_sessions) > 0L) {
    stop(
      "Insufficient observations in sessions: ",
      paste(empty_sessions, collapse = ", "),
      call. = FALSE
    )
  }

  log_prices <- log(as.matrix(sessions$overlap[, c("futures", "spot")]))
  colnames(log_prices) <- c("futures", "spot")
  log_returns <- diff(log_prices)
  zero_return_fraction <- colMeans(log_returns == 0)

  vecm_fit <- vecm(log_prices, K = K)
  his_upper <- his(vecm_fit)

  reversed_fit <- vecm(log_prices[, 2:1, drop = FALSE], K = K)
  his_lower <- rev(his(reversed_fit))
  names(his_lower) <- c("futures", "spot")
  his_midpoint <- (his_upper + his_lower) / 2

  common <- common_price(vecm_fit)
  common_returns <- diff(common$common_price)
  padding <- nrow(log_prices) - length(common_returns)
  if (padding < 0L) {
    stop("The extracted common price is longer than the input series.", call. = FALSE)
  }
  efficient_returns <- c(rep(0, padding), common_returns)

  overlap_lengths <- c(
    overlap_pre = nrow(sessions$overlap_pre),
    overlap_core = nrow(sessions$overlap_core),
    overlap_post = nrow(sessions$overlap_post_1) + nrow(sessions$overlap_post_2)
  )
  return_parts <- .slice_returns(efficient_returns, overlap_lengths)
  overlapping_variance <- vapply(
    return_parts,
    realized_variance,
    numeric(1L)
  )

  kernel_args <- list(
    kernel_type = kernel_type,
    bandwidth_constant = bandwidth_constant,
    align_by = align_by,
    align_period = align_period
  )
  non_overlapping_variance <- c(
    futures_pre = do.call(
      realized_kernel,
      c(list(sessions$futures_pre, "futures"), kernel_args)
    ),
    spot_maintenance = do.call(
      realized_kernel,
      c(list(sessions$spot_maintenance, "spot"), kernel_args)
    ),
    futures_post = do.call(
      realized_kernel,
      c(list(sessions$futures_post, "futures"), kernel_args)
    )
  )

  combined <- combine_weights(
    overlapping_variance = overlapping_variance,
    non_overlapping_variance = non_overlapping_variance
  )
  weights <- combined$variance_weights
  futures_only_weight <- weights[["futures_pre"]] + weights[["futures_post"]]
  spot_only_weight <- weights[["spot_maintenance"]]
  daily_cwis <- combined$overlapping_weight * his_midpoint +
    c(futures = futures_only_weight, spot = spot_only_weight)

  day <- unique(day_data$trading_day)
  if (length(day) != 1L) {
    stop("`day_data` must contain exactly one trading day.", call. = FALSE)
  }

  structure(
    list(
      date = day,
      his_lower = his_lower,
      his_upper = his_upper,
      his_midpoint = his_midpoint,
      residual_correlation = common$residual_correlation,
      zero_return_fraction = zero_return_fraction,
      realized_variance = combined$realized_variance,
      variance_weights = weights,
      overlapping_weight = combined$overlapping_weight,
      cwis = daily_cwis,
      n_observations = nrow(day_data),
      n_overlap = nrow(sessions$overlap)
    ),
    class = "cwis_day_result"
  )
}
