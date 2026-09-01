#' Split standardized observations by trading day
#'
#' @param data Standardized data returned by
#'   [validate_and_standardize_input()].
#'
#' @return A named list of daily data frames.
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
