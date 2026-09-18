.default_session_boundaries <- function() {
  c(
    overlap_open = 4 * 60 * 60,
    core_open = 9 * 60 * 60 + 30 * 60,
    core_close_exclusive = 16 * 60 * 60 + 1,
    maintenance_start = 17 * 60 * 60,
    overlap_resume = 18 * 60 * 60,
    overlap_close = 20 * 60 * 60
  )
}

.seconds_from_midnight <- function(datetime, tz) {
  value <- as.POSIXlt(datetime, tz = tz)
  value$hour * 3600 + value$min * 60 + value$sec
}

#' Separate a trading day into market sessions
#'
#' @description
#' Divides one standardized trading-day data frame into overlapping and
#' single-market sessions according to local clock time.
#'
#' @details
#' The function assigns every observation to one of seven non-overlapping
#' component sessions. With the default boundaries, these sessions are:
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
#' - `futures_pre`: before 04:00:00;
#' - `overlap_pre`: from 04:00:00 to 09:30:00;
#' - `overlap_core`: from 09:30:00 to 16:00:01;
#' - `overlap_post_1`: from 16:00:01 to 17:00:00;
#' - `spot_maintenance`: from 17:00:00 to 18:00:00;
#' - `overlap_post_2`: from 18:00:00 to 20:00:00;
#' - `futures_post`: from 20:00:00 onward.
#'
#' Except for the first and last sessions, intervals include their lower
#' boundary and exclude their upper boundary. The combined `overlap` data
#' frame is formed by chronologically joining `overlap_pre`, `overlap_core`,
#' `overlap_post_1`, and `overlap_post_2`.
#'
#' The default value of `core_close_exclusive` is 16:00:01. It was chosen for
#' one-second observations so that 16:00:00 remains in `overlap_core` and the
#' following second begins `overlap_post_1`. With sub-second data, observations
#' between 16:00:00 and 16:00:01 are therefore also assigned to
#' `overlap_core`. Users analysing millisecond data should verify that this
#' convention is appropriate and, if necessary, supply sampling-frequency-
#' appropriate `boundaries`.
#'
#' Session membership is determined in `tz`. Specifying a different time zone
#' changes the local clock time used for the allocation but does not change the
#' underlying timestamp instants.
#'
#' @param day_data A standardized data frame for one trading day, normally one
#'   element returned by [split_trading_days()]. It must contain `datetime`,
#'   `trading_day`, `futures`, and `spot`.
#' @param boundaries A strictly increasing named numeric vector containing
#'   seconds from local midnight. It must contain, in order, the names
#'   `overlap_open`, `core_open`, `core_close_exclusive`,
#'   `maintenance_start`, `overlap_resume`, and `overlap_close`. All boundaries
#'   must be finite and lie strictly between 00:00:00 and 24:00:00.
#' @param tz A time-zone string used to convert timestamps to local clock time.
#'   By default, the `"tz"` attribute stored by
#'   [validate_and_standardize_input()] is used. If that attribute is absent,
#'   `"America/New_York"` is used.
#'
#' @return
#' A named list containing the following data frames:
#'
#' - `futures_pre`;
#' - `overlap_pre`;
#' - `overlap_core`;
#' - `overlap_post_1`;
#' - `spot_maintenance`;
#' - `overlap_post_2`;
#' - `futures_post`;
#' - `overlap`, containing all four overlapping-session components.
#'
#' Each data frame retains the columns of `day_data`, has reset row names, and
#' carries the time zone used by the function in its `"tz"` attribute. Sessions
#' without observations are returned as empty data frames.
#'
#' @references
#' Dimpfl, T. and Schweikert, K. (2023). Information shares for markets with
#' partially overlapping trading hours. \emph{Journal of Banking & Finance},
#' 154, 106970. \doi{10.1016/j.jbankfin.2023.106970}
#'
#' @seealso
#' [validate_and_standardize_input()] for preparing the input data;
#' [split_trading_days()] for creating daily data frames; [daily_cwis()] for
#' calculating daily information shares from the resulting sessions.
#'
#' @examples
#' example_data <- data.frame(
#'   timestamp = c(
#'     "2026-01-05 03:59:59",
#'     "2026-01-05 04:00:00",
#'     "2026-01-05 09:30:00",
#'     "2026-01-05 16:00:00",
#'     "2026-01-05 16:00:01",
#'     "2026-01-05 17:00:00",
#'     "2026-01-05 18:00:00",
#'     "2026-01-05 20:00:00"
#'   ),
#'   futures_price = 5000:5007,
#'   etf_price = 500:507
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
#' day_data <- split_trading_days(standardized)[[1]]
#' sessions <- split_trading_sessions(day_data)
#'
#' names(sessions)
#' vapply(sessions, nrow, integer(1))
#'
#' @export
split_trading_sessions <- function(
    day_data,
    boundaries = .default_session_boundaries(),
    tz = attr(day_data, "tz") %||% "America/New_York") {
  required_columns <- c("datetime", "trading_day", "futures", "spot")
  if (!is.data.frame(day_data) || !all(required_columns %in% names(day_data))) {
    stop("`day_data` must be a standardized daily data frame.", call. = FALSE)
  }

  expected_names <- names(.default_session_boundaries())
  if (!is.numeric(boundaries) || !identical(names(boundaries), expected_names) ||
      any(!is.finite(boundaries)) || any(diff(boundaries) <= 0) ||
      boundaries[[1L]] <= 0 ||
      boundaries[[length(boundaries)]] >= 24 * 60 * 60) {
    stop(
      "`boundaries` must be an increasing named vector matching the default names.",
      call. = FALSE
    )
  }

  seconds <- .seconds_from_midnight(day_data$datetime, tz = tz)
  take <- function(index) {
    value <- day_data[index, , drop = FALSE]
    rownames(value) <- NULL
    attr(value, "tz") <- tz
    value
  }

  sessions <- list(
    futures_pre = take(seconds < boundaries[["overlap_open"]]),
    overlap_pre = take(
      seconds >= boundaries[["overlap_open"]] &
        seconds < boundaries[["core_open"]]
    ),
    overlap_core = take(
      seconds >= boundaries[["core_open"]] &
        seconds < boundaries[["core_close_exclusive"]]
    ),
    overlap_post_1 = take(
      seconds >= boundaries[["core_close_exclusive"]] &
        seconds < boundaries[["maintenance_start"]]
    ),
    spot_maintenance = take(
      seconds >= boundaries[["maintenance_start"]] &
        seconds < boundaries[["overlap_resume"]]
    ),
    overlap_post_2 = take(
      seconds >= boundaries[["overlap_resume"]] &
        seconds < boundaries[["overlap_close"]]
    ),
    futures_post = take(seconds >= boundaries[["overlap_close"]])
  )

  sessions$overlap <- do.call(
    rbind,
    sessions[c("overlap_pre", "overlap_core", "overlap_post_1", "overlap_post_2")]
  )
  rownames(sessions$overlap) <- NULL
  attr(sessions$overlap, "tz") <- tz
  sessions
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) y else x
}
