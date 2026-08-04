#'@export

cwis <- function(
    data,
    sessions,
    vecm_lags,
    kernel,
    sampling_interval) {

  parts <- split_sessions(data, sessions)

  overlap_data <- collect_overlap_periods(parts)

  vecm_fit <- fit_overlap_vecm(
    overlap_data,
    lags = vecm_lags
  )

  his <- hasbrouck_is(vecm_fit)

  efficient_ret <- efficient_returns(
    vecm_fit,
    overlap_data
  )

  period_iv <- estimate_period_variances(
    parts = parts,
    efficient_returns = efficient_ret,
    kernel = kernel,
    sampling_interval = sampling_interval
  )

  weights <- contribution_weights(period_iv)

  final_cwis <- combine_cwis(
    his = his$midpoint,
    weights = weights,
    sessions = sessions
  )

  list(
    cwis = final_cwis,
    his = his,
    period_variance = period_iv,
    weights = weights,
    diagnostics = vecm_diagnostics(vecm_fit)
  )
}
