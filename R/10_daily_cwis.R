.slice_returns <- function(returns, lengths) {
  endpoints <- cumsum(lengths)
  starts <- c(1L, endpoints[-length(endpoints)] + 1L)
  parts <- Map(function(start, end) returns[start:end], starts, endpoints)
  stats::setNames(parts, names(lengths))
}

#' Calculate CWIS for one trading day
#'
#' @param day_data One standardized trading day.
#' @param K VECM lag order.
#' @param kernel_type,bandwidth_constant,align_by,align_period Controls for
#'   single-market realized kernels.
#' @param session_boundaries Named session-boundary vector.
#'
#' @return A `cwis_day_result` list.
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
