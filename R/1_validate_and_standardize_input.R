#' Validate and standardize CWIS input data
#'
#' @description
#' Validates, scales, sorts, and standardizes two synchronized price series
#' before they are used in the contribution-weighted information-share
#' calculation.
#'
#' The first series represents a futures market. The second represents the
#' corresponding spot or cash-market price. In the empirical application of
#' Dimpfl and Schweikert (2023), an ETF price is used as the cash-market proxy.
#'
#' @details
#' The function performs the following operations:
#'
#' - verifies that `data` is a data frame containing the requested columns;
#' - requires both price columns to be numeric;
#' - parses character or `POSIXt` timestamps in `tz`;
#' - optionally removes rows with missing or unparseable timestamps;
#' - multiplies the spot-price series by `spot_multiplier`;
#' - rejects observed prices that are non-finite or non-positive;
#' - sorts observations chronologically;
#' - rejects duplicate timestamps; and
#' - assigns a local calendar trading date to every observation.
#'
#' Individual missing prices are retained because one market may be inactive
#' during a non-overlapping session. However, neither price series may be
#' entirely missing. Missing prices required in a subsequent VECM or
#' realized-kernel calculation can cause that trading day to fail.
#'
#' `futures_col` and `spot_col` identify the economic roles of the two input
#' series; the corresponding columns in `data` may have any names. For example,
#' the NASDAQ-100 application may use an E-mini futures column for
#' `futures_col` and a QQQ ETF column for `spot_col`.
#'
#' `spot_multiplier` is product-specific. The default value of `10` corresponds
#' to the bundled S&P 500 example. Users should set it to `1` when the two
#' input series are already on the intended scales, or supply another
#' appropriate conversion factor.
#'
#' `tz` determines timestamp interpretation, calendar-day assignment, and
#' subsequent trading-session allocation. A valid Olson time-zone name such
#' as `"America/New_York"` is recommended. Fractional seconds are supported
#' when they are present in character timestamps.
#'
#' @param data A data frame containing one timestamp column and two
#'   synchronized numeric price columns representing the futures market and
#'   the corresponding spot, cash, or ETF market.
#'
#' @param datetime_col A single character string naming the timestamp column.
#'   Its values must be character or inherit from `POSIXt`.
#'
#' @param futures_col A single character string naming the numeric
#'   futures-price column. This series is assumed to be observed during
#'   futures-only trading sessions.
#'
#' @param spot_col A single character string naming the numeric spot-, cash-,
#'   or ETF-price column. This series is assumed to be observed during the
#'   spot-only trading session.
#'
#' @param spot_multiplier A finite numeric scalar applied to the spot-price
#'   series during standardization, before logarithms  are calculated.
#'   The multiplier should place the spot and futures prices on
#'   economically comparable scales. The default is `10`, corresponding to
#'   the bundled S&P 500 futures and SPY example; use `1` when the supplied
#'   series are already on comparable scales.
#'
#' @param tz A single character string specifying the IANA/Olson time zone
#'   used to parse timestamps, assign local trading dates, and interpret
#'   intraday session boundaries. Defaults to `"America/New_York"`,
#'   consistent with the U.S. market application considered in the paper.
#'
#' @param drop_missing_datetime A logical scalar. If `TRUE`, rows with missing
#'   or unparseable timestamps are removed. If `FALSE`, their presence causes
#'   an error.
#'
#' @section Input requirements:
#'
#' `data` must be a data frame containing one timestamp column and two numeric
#' price columns. Their names are supplied through `datetime_col`,
#' `futures_col`, and `spot_col`.
#'
#' The timestamp column must be either a character vector or an object
#' inheriting from `POSIXt`. Character timestamps are interpreted in `tz`.
#' Supported representations include:
#'
#' - `"YYYY-MM-DD HH:MM:SS"`;
#' - `"YYYY/MM/DD HH:MM:SS"`;
#' - `"YYYY-MM-DDTHH:MM:SS"`;
#' - `"YYYY-MM-DDTHH:MM:SSZ"`.
#'
#' Fractional seconds are supported, for example
#' `"2026-01-05 09:30:00.125"`.
#'
#' Missing or unparseable timestamps are removed when
#' `drop_missing_datetime = TRUE`. If `drop_missing_datetime = FALSE`, their
#' presence causes an error. The remaining observations are sorted
#' chronologically. Duplicate timestamps are not combined and cause the
#' function to stop.
#'
#' Both price columns must be numeric. Individual prices may be missing, but
#' neither price series may be entirely missing. Observed prices must be finite
#' and strictly positive after applying `spot_multiplier`. Missing prices are
#' retained and are not imputed.
#'
#' @section Dates and time zone:
#'
#' `tz` determines how character timestamps are interpreted and how
#' `trading_day` is assigned. It should normally be a valid IANA/Olson
#' time-zone name, such as `"America/New_York"`, `"Europe/Berlin"`,
#' `"Europe/London"`, `"Asia/Shanghai"`, or `"UTC"`. Available names can be
#' inspected with `OlsonNames()`.
#'
#' The `trading_day` column represents the calendar date of each observation
#' in `tz`. Dates absent from the input are not inserted. Similarly, a date from
#' which all observations are removed during timestamp validation will not
#' appear in the returned data.
#'
#' Time-zone abbreviations such as `"EST"` or `"CET"` are best avoided because
#' they may not apply daylight-saving-time rules as intended.
#'
#' @return
#' A data frame with observations sorted by `datetime` and row names reset.
#' It contains:
#'
#' \describe{
#'   \item{datetime}{
#'     A `POSIXct` timestamp represented in `tz`.
#'   }
#'   \item{trading_day}{
#'     A `Date` derived from the local calendar date in `tz`.
#'   }
#'   \item{futures}{
#'     The standardized futures-price series.
#'   }
#'   \item{spot}{
#'     The spot-price series after multiplication by `spot_multiplier`.
#'   }
#' }
#'
#' The selected time-zone string is also stored in the `"tz"` attribute of
#' the returned data frame.
#'
#' @references
#' Dimpfl, T. and Schweikert, K. (2023). Information shares for markets with
#' partially overlapping trading hours. \emph{Journal of Banking & Finance},
#' 154, 106970. \doi{10.1016/j.jbankfin.2023.106970}
#'
#' @seealso
#' [split_trading_days()] for dividing the standardized data by local date,
#' [split_trading_sessions()] for assigning intraday sessions, and
#' [cwis()] for the complete multi-day CWIS calculation.
#'
#' @examples
#' example_data <- data.frame(
#'   timestamp = c(
#'     "2026-01-05 09:30:01.250",
#'     "2026-01-05 09:30:00.500",
#'     NA_character_
#'   ),
#'   futures_price = c(5001, 5000, 5002),
#'   etf_price = c(500.1, 500.0, 500.2)
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
#' standardized
#' attr(standardized, "tz")
#'
#' @export
validate_and_standardize_input <- function(
    data,
    datetime_col,
    futures_col,
    spot_col,
    spot_multiplier = 10,
    tz,
    drop_missing_datetime = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  required <- c(datetime_col, futures_col, spot_col)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns: ", paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(spot_multiplier) || length(spot_multiplier) != 1L ||
      !is.finite(spot_multiplier)) {
    stop("`spot_multiplier` must be one finite number.", call. = FALSE)
  }
  if (!is.character(tz) || length(tz) != 1L || !nzchar(tz)) {
    stop("`tz` must be one non-empty time-zone string.", call. = FALSE)
  }

  futures_raw <- data[[futures_col]]
  spot_raw <- data[[spot_col]]
  if (!is.numeric(futures_raw) || !is.numeric(spot_raw)) {
    stop("Both price columns must be numeric.", call. = FALSE)
  }

  datetime <- data[[datetime_col]]
  if (inherits(datetime, "POSIXt")) {
    datetime <- as.POSIXct(datetime, tz = tz)
  } else if (is.character(datetime)) {
    datetime <- as.POSIXct(
      datetime,
      tz = tz,
      tryFormats = c(
        "%Y-%m-%d %H:%M:%OS",
        "%Y/%m/%d %H:%M:%OS",
        "%Y-%m-%dT%H:%M:%OS",
        "%Y-%m-%dT%H:%M:%OSZ"
      )
    )
  } else {
    stop("The timestamp column must be character or POSIXt.", call. = FALSE)
  }

  keep <- !is.na(datetime)
  if (!all(keep) && !isTRUE(drop_missing_datetime)) {
    stop("The timestamp column contains missing or unparseable values.", call. = FALSE)
  }

  datetime <- datetime[keep]
  futures <- futures_raw[keep]
  spot <- spot_raw[keep] * spot_multiplier
  if (length(datetime) == 0L) {
    stop("No usable observations remain after timestamp validation.", call. = FALSE)
  }
  invalid_futures <- !is.na(futures) & (!is.finite(futures) | futures <= 0)
  invalid_spot <- !is.na(spot) & (!is.finite(spot) | spot <= 0)
  if (any(invalid_futures) || any(invalid_spot)) {
    stop(
      "Observed prices must be finite and strictly positive after scaling.",
      call. = FALSE
    )
  }
  if (all(is.na(futures)) || all(is.na(spot))) {
    stop("Neither price series may be entirely missing.", call. = FALSE)
  }

  out <- data.frame(
    datetime = datetime,
    trading_day = as.Date(format(datetime, tz = tz, usetz = FALSE)),
    futures = as.numeric(futures),
    spot = as.numeric(spot),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$datetime), , drop = FALSE]
  rownames(out) <- NULL

  if (anyDuplicated(out$datetime)) {
    stop("The timestamp column contains duplicate observations.", call. = FALSE)
  }

  attr(out, "tz") <- tz
  out
}
