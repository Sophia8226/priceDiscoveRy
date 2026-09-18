#' Estimate 24-hour contribution-weighted information shares
#'
#' @description
#' Estimates daily and aggregate contribution-weighted information shares
#' (CWIS) for two markets with partially overlapping trading hours. The
#' function combines Hasbrouck information shares from periods in which both
#' markets trade with variance contributions from periods in which only one
#' market trades.
#'
#' @details
#' `cwis()` is the main user-level function of the package. It validates and
#' standardizes the input data, divides the observations into trading days,
#' calculates a CWIS estimate for each usable day, and aggregates the daily
#' estimates and diagnostics.
#'
#' The calculation follows the framework of Dimpfl and Schweikert (2023). The
#' complete workflow is summarized below:
#'
#' \if{html}{\figure{package-workflow.png}{options: style="display:block; margin-left:auto; margin-right:auto; max-width:100\%; height:auto;" alt="Workflow of the priceDiscoveRy package"}}
#' \if{latex}{
#' \out{\begin{center}}
#' \figure{package-workflow.png}{options: width=6in}
#' \out{\end{center}}
#' }
#'
#' The calculation proceeds in four main stages:
#'
#' 1. [validate_and_standardize_input()] checks and standardizes the complete
#'    input data.
#' 2. [split_trading_days()] divides the standardized observations into
#'    individual trading days.
#' 3. [daily_cwis()] calculates the CWIS and diagnostics for each trading day.
#' 4. [aggregate_cwis()] summarizes the daily results across trading days.
#'
#' For each trading day, [daily_cwis()] divides the observations into
#' overlapping and single-market sessions. It estimates the midpoint Hasbrouck
#' information shares for overlapping trading and the variance contribution
#' of each intraday period. During a single-market session, the open market is
#' assigned an information share of one and the closed market an information
#' share of zero. The resulting components are combined using normalized
#' variance weights.
#'
#' The daily futures and spot CWIS estimates sum to one, apart from numerical
#' rounding.
#'
#' Details of the session definitions, VECM, common efficient price, Hasbrouck
#' information shares, variance estimators, and variance weights are documented
#' in [split_trading_sessions()], [vecm()], [common_price()], [his()],
#' [realized_variance()], [realized_kernel()], and [combine_weights()].
#'
#' @section Trading sessions:
#'
#' \strong{Trading-session structure:}
#'
#' \if{html}{\figure{trading-sessions.svg}{options: width="1000" style="display:block; margin-left:auto; margin-right:auto; max-width:100%; height:auto;" alt="Trading-session structure"}}
#' \if{latex}{
#' \out{\begin{center}}
#' \figure{trading-sessions.pdf}{options: width=6in}
#' \out{\end{center}}
#' }
#'
#' With the default `session_boundaries`, each calendar day in `tz` is divided
#' into the following market configurations:
#'
#' - 00:00--04:00: futures market only;
#' - 04:00--17:00: overlapping trading;
#' - 17:00--18:00: spot market only;
#' - 18:00--20:00: overlapping trading;
#' - 20:00--24:00: futures market only.
#'
#' The overlapping observations are further divided into pre-core, core, and
#' post-core subperiods for the variance calculation. However, a single VECM
#' is estimated using the combined observations from all overlapping
#' subperiods, thereby imposing common VECM parameters across these periods.
#'
#' These default boundaries reproduce the futures and ETF trading schedule
#' considered by Dimpfl and Schweikert (2023). For other assets or exchanges,
#' users should specify boundaries that reflect the applicable trading
#' schedule. See [split_trading_sessions()] for the precise interval
#' definitions.
#'
#' @section Input requirements:
#'
#' `data` must be a data frame containing one timestamp column and two numeric
#' price columns. The column names are supplied as character strings through
#' `datetime_col`, `futures_col`, and `spot_col`.
#'
#' The timestamp column must be character or inherit from `POSIXt`. Character
#' timestamps are interpreted in `tz`. Missing or unparseable timestamps are
#' removed before the daily calculations. The remaining observations are
#' sorted chronologically, but duplicate timestamps cause the complete
#' calculation to stop.
#'
#' Prices must be finite and strictly positive after applying
#' `spot_multiplier`. Missing prices are not imputed. Although individual
#' missing prices can pass the initial standardization, the affected trading
#' day will generally fail when complete prices are required by the VECM or
#' realized-kernel calculation.
#'
#' The two price series should represent synchronized observations of the same
#' underlying asset, or assets linked by a stable law-of-one-price relationship.
#' The current session logic assumes that the series identified by
#' `futures_col` trades during the futures-only periods and the series
#' identified by `spot_col` trades during the spot-only period.
#'
#' Every session required by [daily_cwis()] must contain at least two
#' observations. The combined overlapping period must contain more than
#' `K + 2` complete observations, although substantially more observations are
#' normally required for stable VECM estimation.
#'
#' @section Time zone and sampling:
#'
#' `tz` controls timestamp parsing, trading-day assignment, and the
#' interpretation of the intraday session boundaries. It should be a valid
#' Olson time-zone name, such as `"America/New_York"`, `"America/Chicago"`,
#' `"Europe/London"`, `"Europe/Paris"`, or `"UTC"`.
#'
#' Trading days run from local midnight to the following local midnight in
#' `tz`. Users should check the treatment of daylight-saving transitions,
#' overnight trading conventions, and exchange holidays before applying the
#' default boundaries to another market.
#'
#' The defaults `align_by = "seconds"` and `align_period = 1L` align
#' single-market observations to one-second intervals. They do not reproduce
#' the 10-millisecond sampling frequency used in the empirical application of
#' Dimpfl and Schweikert (2023). Users may change the alignment settings when
#' supported by [highfrequency::rKernelCov()] and appropriate for their data.
#'
#' @section Choice of the VECM lag order:
#'
#' In this implementation, `K` is passed directly to [urca::ca.jo()] and
#' denotes the lag order of the variables in the underlying level VAR. The
#' corresponding standard Johansen VECM contains `K - 1` differenced lags.
#' Thus, `K = 10L` produces nine short-run lag matrices.
#'
#' Users may select another value of `K`, subject to `K >= 2` and the
#' availability of sufficient observations. Lag selection should consider the
#' sampling frequency, remaining serial dependence, and numerical stability.
#'
#' The reference paper uses a different specification of the short-run
#' dynamics. At a sampling frequency of 10 milliseconds, Dimpfl and
#' Schweikert (2023) set \eqn{\delta = 4}, with time-scale boundaries
#' \eqn{k_1 = 1}, \eqn{k_2 = 10}, \eqn{k_3 = 100}, and
#' \eqn{k_4 = 1000}. The resulting four HAR coefficient blocks cover lagged
#' price changes over a maximum horizon of ten seconds. In contrast, the
#' present function estimates `K - 1` separate short-run coefficient matrices
#' using the standard Johansen VECM. It therefore implements the VECM component
#' of the CWIS framework but does not exactly reproduce the paper's
#' high-resolution HAR-VECM specification.
#'
#' @section Failed trading days:
#'
#' If `continue_on_error = TRUE`, an error arising during a particular daily
#' calculation is captured and the date is retained in the output with
#' `status = "failed"`, the corresponding error message, and missing numerical
#' estimates. Calculations then continue for the remaining dates.
#'
#' If `continue_on_error = FALSE`, the first daily error stops the entire
#' calculation.
#'
#' Errors occurring before the data are divided into days, including invalid
#' column names, invalid timestamp types, duplicate timestamps, and invalid
#' observed prices, are not handled by `continue_on_error` and always stop the
#' function.
#'
#' @inheritParams validate_and_standardize_input data datetime_col futures_col spot_col spot_multiplier tz
#'
#' @inheritParams vecm K
#'
#' @inheritParams realized_kernel kernel_type bandwidth_constant align_by align_period
#'
#' @inheritParams daily_cwis session_boundaries
#'
#' @param continue_on_error Logical scalar controlling the treatment of errors
#'   raised during individual daily calculations. See the Failed trading days
#'   section.
#'
#' @return
#' An object of class `cwis_result`, represented by a list with the following
#' components:
#'
#' \describe{
#'   \item{daily}{
#'     A data frame with one row per supplied trading day. It contains `date`,
#'     `status`, `error`, the futures and spot HIS midpoints, the futures and
#'     spot CWIS estimates, the total overlapping-period weight, the
#'     contemporaneous VECM residual correlation, the zero-return fractions,
#'     and the total and overlapping observation counts. Numerical results are
#'     `NA` for failed days.
#'   }
#'   \item{summary}{
#'     A data frame reporting the mean and standard deviation of the successful
#'     daily HIS midpoints and CWIS estimates for both markets. Failed days are
#'     excluded.
#'   }
#'   \item{diagnostics}{
#'     A data frame reporting the number of supplied, successful, and failed
#'     days, the mean and standard deviation of the overlapping-period weight,
#'     and the mean contemporaneous residual correlation.
#'   }
#'   \item{variance_weights}{
#'     A data frame containing the mean and standard deviation of each
#'     period-specific variance weight across successful days. It is empty if
#'     no daily calculation succeeds.
#'   }
#'   \item{details}{
#'     A named list containing the complete `cwis_day_result` object for every
#'     successful day. Failed entries contain only the trading date and error
#'     message.
#'   }
#'   \item{settings}{
#'     A list recording the column mappings, scaling factor, time zone, VECM
#'     lag order, kernel and alignment controls, and session boundaries used in
#'     the calculation.
#'   }
#' }
#'
#' @references
#' Dimpfl, T. and Schweikert, K. (2023). Information shares for markets with
#' partially overlapping trading hours. \emph{Journal of Banking & Finance},
#' 154, 106970. \doi{10.1016/j.jbankfin.2023.106970}
#'
#' Hasbrouck, J. (1995). One security, many markets: Determining the
#' contributions to price discovery.\emph{The Journal of Finance}, 50(4),
#' 1175--1199.\doi{10.2307/2329348}
#'
#' Dias, G. F. and Schweikert, K. (2022). Integrated variance estimation for
#' assets traded in multiple venues. \emph{SSRN Working Paper}, 1--48.
#' \url{https://ssrn.com/abstract=4253762}
#'
#' Wang, J. and Yang, M. (2011). Housewives of Tokyo versus the gnomes of
#' Zurich: Measuring price discovery in sequential markets.
#' \emph{Journal of Financial Markets}, 14(1), 82--108. \doi{10.1016/j.finmar.2010.08.002}
#'
#' Barndorff-Nielsen, O. E., Hansen, P. R., Lunde, A., and Shephard, N. (2008).
#' Designing realized kernels to measure the ex post variation of equity prices
#' in the presence of noise. \emph{Econometrica}, 76(6), 1481--1536. \doi{10.3982/ECTA6495}
#'
#' Barndorff-Nielsen, O. E., Hansen, P. R., Lunde, A., and Shephard, N. (2009).
#' Realized kernels in practice: Trades and quotes.
#' \emph{The Econometrics Journal}, 12(3), 1--32. \doi{10.1111/j.1368-423X.2008.00275.x}
#'
#' Hasbrouck, J. (2021). Price discovery in high resolution.
#' \emph{Journal of Financial Econometrics}, 19(3), 395--430.
#' \doi{10.1093/jjfinec/nbz027}
#'
#' Buccheri, G., Bormetti, G., Corsi, F., and Lillo, F. (2021). Comment on:
#' Price discovery in high resolution. \emph{Journal of Financial Econometrics},
#' 19(3), 439--451. \doi{10.1093/jjfinec/nbz008}
#'
#' @seealso
#' [daily_cwis()] for the complete single-day calculation;
#' [validate_and_standardize_input()] for input validation;
#' [split_trading_sessions()] for session definitions;
#' [vecm()] and [common_price()] for estimation of the common efficient price;
#' [his()] for Hasbrouck information shares;
#' [realized_variance()] and [realized_kernel()] for period variances;
#' [combine_weights()] for variance weights;
#' [aggregate_cwis()] for aggregation across days; and
#' [plot.cwis_result()] for plotting daily results.
#'
#' @examples
#' \donttest{
#' set.seed(123)
#'
#' tz <- "America/New_York"
#' datetime <- seq(
#'   from = as.POSIXct("2024-01-03 00:00:00", tz = tz),
#'   to = as.POSIXct("2024-01-03 23:59:00", tz = tz),
#'   by = "1 min"
#' )
#'
#' n <- length(datetime)
#' common_trend <- cumsum(stats::rnorm(n, sd = 0.0002))
#' spread <- as.numeric(stats::arima.sim(
#'   model = list(ar = 0.8),
#'   n = n,
#'   sd = 0.0001
#' ))
#'
#' example_data <- data.frame(
#'   datetime = datetime,
#'   futures_price = exp(log(5000) + common_trend + spread / 2),
#'   spot_price = exp(log(5000) + common_trend - spread / 2) / 10
#' )
#'
#' result <- cwis(
#'   data = example_data,
#'   datetime_col = "datetime",
#'   futures_col = "futures_price",
#'   spot_col = "spot_price",
#'   spot_multiplier = 10,
#'   tz = tz,
#'   K = 2L,
#'   align_by = "minutes",
#'   align_period = 1L,
#'   continue_on_error = FALSE
#' )
#'
#' print(result)
#' result$daily
#' result$summary
#' result$diagnostics
#' }
#'
#' @export
cwis <- function(
    data,
    datetime_col,
    futures_col,
    spot_col,
    spot_multiplier = 10,
    tz,
    K = 10L,
    kernel_type = "ModifiedTukeyHanning",
    bandwidth_constant = NULL,
    align_by = "seconds",
    align_period = 1L,
    session_boundaries = .default_session_boundaries(),
    continue_on_error = TRUE) {
  standardized <- validate_and_standardize_input(
    data = data,
    datetime_col = datetime_col,
    futures_col = futures_col,
    spot_col = spot_col,
    spot_multiplier = spot_multiplier,
    tz = tz
  )
  days <- split_trading_days(standardized)

  results <- lapply(days, function(day_data) {
    day <- unique(day_data$trading_day)
    calculate <- function() {
      daily_cwis(
        day_data = day_data,
        K = K,
        kernel_type = kernel_type,
        bandwidth_constant = bandwidth_constant,
        align_by = align_by,
        align_period = align_period,
        session_boundaries = session_boundaries
      )
    }

    if (isTRUE(continue_on_error)) {
      tryCatch(
        calculate(),
        error = function(condition) {
          list(date = day, error = conditionMessage(condition))
        }
      )
    } else {
      calculate()
    }
  })

  aggregated <- aggregate_cwis(results)
  structure(
    list(
      daily = aggregated$daily,
      summary = aggregated$information_share,
      diagnostics = aggregated$diagnostics,
      variance_weights = aggregated$variance_weights,
      details = results,
      settings = list(
        datetime_col = datetime_col,
        futures_col = futures_col,
        spot_col = spot_col,
        spot_multiplier = spot_multiplier,
        tz = tz,
        K = K,
        kernel_type = kernel_type,
        bandwidth_constant = bandwidth_constant,
        align_by = align_by,
        align_period = align_period,
        session_boundaries = session_boundaries
      )
    ),
    class = "cwis_result"
  )
}

#' @export
print.cwis_result <- function(x, ...) {
  successful <- sum(x$daily$status == "ok")
  failed <- sum(x$daily$status == "failed")
  cat("24-hour CWIS result\n")
  cat("  Successful days:", successful, "\n")
  cat("  Failed days:    ", failed, "\n\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

