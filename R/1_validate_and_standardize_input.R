#' Validate and standardize CWIS input data
#'
#' @param data A data frame containing timestamps and two price series.
#' @param datetime_col Name of the timestamp column.
#' @param futures_col Name of the futures-price column.
#' @param spot_col Name of the spot-price column.
#' @param spot_multiplier Multiplier applied to the spot price.
#' @param tz Time zone used to parse timestamps and define trading days.
#' @param drop_missing_datetime Whether rows with missing timestamps are removed.
#'
#' @return A data frame with columns `datetime`, `trading_day`, `futures`, and
#'   `spot`.
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
