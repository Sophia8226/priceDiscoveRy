#' Estimate contribution-weighted information shares
#'
#' This is the package entry point. It validates the input, splits it into
#' trading days, estimates each day, and returns daily and multi-day results.
#'
#' @param data A data frame containing timestamps and futures/spot prices.
#' @param datetime_col,futures_col,spot_col Input column names.
#' @param spot_multiplier Multiplier applied to the spot series. The default 10
#'   reproduces the original script.
#' @param tz Time zone defining timestamps and trading days.
#' @param K VECM lag order.
#' @param kernel_type,bandwidth_constant,align_by,align_period Controls for
#'   single-market realized-kernel estimates.
#' @param session_boundaries Named seconds-from-midnight boundary vector.
#' @param continue_on_error If `TRUE`, a failed day is recorded and processing
#'   continues. If `FALSE`, the first daily error stops the calculation.
#'
#' @return A `cwis_result` list with `daily`, `summary`, `diagnostics`,
#'   `variance_weights`, `details`, and `settings`.
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

