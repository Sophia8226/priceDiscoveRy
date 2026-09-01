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
#' The default boundaries reproduce the one-second index ranges in the
#' original analysis. In particular, 16:00:00 belongs to the core session and
#' the following second begins the first post-trading overlap session.
#'
#' @param day_data One standardized trading-day data frame.
#' @param boundaries Named numeric vector of seconds from midnight.
#' @param tz Time zone used to interpret timestamps. By default the time zone
#'   stored by [validate_and_standardize_input()] is used.
#'
#' @return A named list containing seven component sessions and one combined
#'   `overlap` data frame.
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
