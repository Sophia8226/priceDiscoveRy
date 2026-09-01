.sum_short_run_coefficients <- function(vecm_fit) {
  dlag <- vecm_fit@lag - 1L
  if (dlag < 1L) {
    stop("The VECM must contain at least one differenced lag.", call. = FALSE)
  }

  gamma <- vecm_fit@GAMMA
  if (ncol(gamma) < 1L + 2L * dlag) {
    stop("Unexpected VECM coefficient layout.", call. = FALSE)
  }
  gamma <- gamma[, -1L, drop = FALSE]
  gamma <- gamma[, seq_len(2L * dlag), drop = FALSE]

  blocks <- lapply(seq_len(dlag), function(index) {
    columns <- (2L * index - 1L):(2L * index)
    gamma[, columns, drop = FALSE]
  })
  Reduce(`+`, blocks)
}

#' Estimate the overlapping-period VECM
#'
#' @param log_prices A two-column matrix or data frame of log futures and spot
#'   prices.
#' @param K Lag order passed to [urca::ca.jo()].
#' @param type,ecdet,spec Arguments passed to [urca::ca.jo()].
#'
#' @return A `ca.jo` object.
#' @export
vecm <- function(
    log_prices,
    K = 10,
    type = "trace",
    ecdet = "none",
    spec = "transitory") {
  log_prices <- as.matrix(log_prices)
  if (!is.numeric(log_prices) || ncol(log_prices) != 2L) {
    stop("`log_prices` must be a numeric two-column object.", call. = FALSE)
  }
  if (nrow(log_prices) <= K + 2L || any(!is.finite(log_prices))) {
    stop("There are too few complete observations for the requested VECM.", call. = FALSE)
  }
  if (any(diag(stats::var(log_prices)) <= 0)) {
    stop("Both log-price series must vary within the overlapping period.", call. = FALSE)
  }

  colnames(log_prices) <- c("futures", "spot")
  urca::ca.jo(
    log_prices,
    type = type,
    ecdet = ecdet,
    K = K,
    spec = spec
  )
}


