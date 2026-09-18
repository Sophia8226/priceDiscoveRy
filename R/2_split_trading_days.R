#' Split standardized observations by trading day
#'
#' @description
#' Splits standardized futures and spot-price observations into separate
#' data frames for each trading day.
#'
#' @details
#' Observations are grouped according to the `trading_day` column created by
#' [validate_and_standardize_input()]. Each resulting data frame contains the
#' observations belonging to one calendar day in the time zone used during
#' standardization.
#'
#' The function does not define intraday trading sessions, resample prices, or
#' remove incomplete trading days. Intraday session assignment is performed
#' separately by [split_trading_sessions()].
#'
#' Row names are reset within each daily data frame. The `"tz"` attribute of
#' the input data is copied to every element of the returned list.
#'
#' @param data A standardized data frame returned by
#'   [validate_and_standardize_input()]. It must contain the columns
#'   `datetime`, `trading_day`, `futures`, and `spot`.
#'
#' @return
#' A named list of data frames, with one element for each trading day present
#' in `data`. List names are the character representations of the corresponding
#' trading dates. Each data frame retains the columns and chronological order
#' of the standardized input and carries the same `"tz"` attribute.
#'
#' @references
#' Dimpfl, T. and Schweikert, K. (2023). Information shares for markets with
#' partially overlapping trading hours. \emph{Journal of Banking & Finance},
#' 154, 106970. \doi{10.1016/j.jbankfin.2023.106970}
#'
#' @seealso
#' [validate_and_standardize_input()] for input validation and standardization;
#' [split_trading_sessions()] for dividing one trading day into intraday
#' sessions; [daily_cwis()] for estimating CWIS for a single trading day.
#'
#' @examples
#' example_data <- data.frame(
#'   timestamp = c(
#'     "2026-01-05 09:30:00",
#'     "2026-01-05 09:30:01",
#'     "2026-01-06 09:30:00",
#'     "2026-01-06 09:30:01"
#'   ),
#'   futures_price = c(5000, 5001, 5010, 5012),
#'   etf_price = c(500.0, 500.1, 501.0, 501.2)
#' )
#'
#' standardized <- validate_and_standardize_input(
#'   data = example_data,
#'   datetime_col = "timestamp",
#'   futures_col = "futures_price",
#'   spot_col = "etf_price",
#'   spot_multiplier = 10,
#'   tz = "America/New_York"
#' )
#'
#' daily_data <- split_trading_days(standardized)
#'
#' names(daily_data)
#' daily_data[[1]]
#'
#' @export
split_trading_days <- function(data) {
  required <- c("datetime", "trading_day", "futures", "spot")
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    stop(
      "`data` must be standardized by `validate_and_standardize_input()`.",
      call. = FALSE
    )
  }

  days <- split(data, data$trading_day, drop = TRUE)
  lapply(days, function(x) {
    rownames(x) <- NULL
    attr(x, "tz") <- attr(data, "tz")
    x
  })
}
