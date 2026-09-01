#' Estimate a generalized realized kernel in a single-market session
#'
#' @param session_data A standardized session data frame.
#' @param price_col Either `"futures"` or `"spot"`.
#' @param kernel_type Kernel passed to [highfrequency::rKernelCov()].
#' @param bandwidth_constant Optional rule-of-thumb multiplier. Defaults to
#'   2.3970 for Modified Tukey-Hanning and 3.5134 for Parzen.
#' @param align_by,align_period Sampling controls passed to
#'   [highfrequency::rKernelCov()].
#'
#' @return One realized-variance estimate.
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
