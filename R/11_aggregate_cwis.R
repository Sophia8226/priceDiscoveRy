.safe_mean <- function(x) {
  if (length(x) == 0L || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

.safe_sd <- function(x) {
  if (sum(!is.na(x)) < 2L) NA_real_ else stats::sd(x, na.rm = TRUE)
}

#' Aggregate daily CWIS estimates
#'
#' @param results A list of successful `cwis_day_result` objects and/or failed
#'   day records created by [cwis()].
#'
#' @return A list with daily results, information-share summary, diagnostic
#'   summary, and variance-weight summary.
#' @export
aggregate_cwis <- function(results) {
  if (!is.list(results)) {
    stop("`results` must be a list.", call. = FALSE)
  }

  daily_rows <- lapply(results, function(result) {
    if (inherits(result, "cwis_day_result")) {
      data.frame(
        date = as.Date(result$date),
        status = "ok",
        error = NA_character_,
        his_futures = unname(result$his_midpoint[["futures"]]),
        his_spot = unname(result$his_midpoint[["spot"]]),
        cwis_futures = unname(result$cwis[["futures"]]),
        cwis_spot = unname(result$cwis[["spot"]]),
        overlapping_weight = result$overlapping_weight,
        residual_correlation = result$residual_correlation,
        zero_return_futures = unname(result$zero_return_fraction[["futures"]]),
        zero_return_spot = unname(result$zero_return_fraction[["spot"]]),
        n_observations = result$n_observations,
        n_overlap = result$n_overlap,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        date = as.Date(result$date),
        status = "failed",
        error = result$error,
        his_futures = NA_real_,
        his_spot = NA_real_,
        cwis_futures = NA_real_,
        cwis_spot = NA_real_,
        overlapping_weight = NA_real_,
        residual_correlation = NA_real_,
        zero_return_futures = NA_real_,
        zero_return_spot = NA_real_,
        n_observations = NA_integer_,
        n_overlap = NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  })
  daily <- if (length(daily_rows) == 0L) data.frame() else do.call(rbind, daily_rows)
  successful <- results[vapply(results, inherits, logical(1L), "cwis_day_result")]

  if (length(successful) == 0L) {
    information_share <- data.frame(
      statistic = c("HIS mean", "HIS SD", "CWIS mean", "CWIS SD"),
      futures = NA_real_,
      spot = NA_real_
    )
    variance_weights <- data.frame()
  } else {
    his <- do.call(rbind, lapply(successful, `[[`, "his_midpoint"))
    daily_cwis <- do.call(rbind, lapply(successful, `[[`, "cwis"))
    information_share <- data.frame(
      statistic = c("HIS mean", "HIS SD", "CWIS mean", "CWIS SD"),
      futures = c(
        .safe_mean(his[, "futures"]), .safe_sd(his[, "futures"]),
        .safe_mean(daily_cwis[, "futures"]), .safe_sd(daily_cwis[, "futures"])
      ),
      spot = c(
        .safe_mean(his[, "spot"]), .safe_sd(his[, "spot"]),
        .safe_mean(daily_cwis[, "spot"]), .safe_sd(daily_cwis[, "spot"])
      )
    )

    weights <- do.call(rbind, lapply(successful, `[[`, "variance_weights"))
    variance_weights <- data.frame(
      period = colnames(weights),
      mean = apply(weights, 2L, .safe_mean),
      sd = apply(weights, 2L, .safe_sd),
      row.names = NULL
    )
  }

  diagnostics <- data.frame(
    statistic = c(
      "days supplied", "days successful", "days failed",
      "mean overlapping weight", "SD overlapping weight",
      "mean residual correlation"
    ),
    value = c(
      length(results),
      length(successful),
      length(results) - length(successful),
      if (nrow(daily)) .safe_mean(daily$overlapping_weight) else NA_real_,
      if (nrow(daily)) .safe_sd(daily$overlapping_weight) else NA_real_,
      if (nrow(daily)) .safe_mean(daily$residual_correlation) else NA_real_
    )
  )

  list(
    daily = daily,
    information_share = information_share,
    diagnostics = diagnostics,
    variance_weights = variance_weights
  )
}

