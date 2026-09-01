#' Plot daily CWIS and Hasbrouck information shares
#'
#' Draws the daily Hasbrouck information-share midpoint and the daily
#' contribution-weighted information share for either the futures or spot
#' market. Only days with `status == "ok"` are plotted. A horizontal benchmark
#' is added at 0.5 by default, and the horizontal axis displays the dates of
#' successful trading days. When there are many successful days, a subset of
#' evenly spaced date labels is shown to keep the axis readable.
#'
#' @param x A `cwis_result` object returned by [cwis()].
#' @param y Unused. This argument is included for compatibility with the
#'   [graphics::plot()] generic.
#' @param market Character string selecting the market to plot. Must be either
#'   `"futures"` or `"spot"`.
#' @param his_colour Colour used for the daily Hasbrouck information-share
#'   midpoint.
#' @param cwis_colour Colour used for the daily contribution-weighted
#'   information share.
#' @param benchmark Numeric scalar giving the horizontal reference level.
#' @param benchmark_colour Colour used for the benchmark line.
#' @param line_width Numeric line width used for the HIS and CWIS series.
#' @param ylim Numeric vector of length two giving the vertical plotting range.
#' @param xlab,ylab Character strings giving the horizontal and vertical axis
#'   labels.
#' @param main Optional plot title.
#' @param date_format Character string passed to [base::format.Date()] to
#'   control the appearance of date labels.
#' @param max_date_labels Maximum number of date labels displayed on the
#'   horizontal axis.
#' @param show_legend Logical scalar indicating whether a legend is displayed.
#' @param legend_position Position passed to [graphics::legend()].
#' @param mar Numeric vector of four plot margins passed to [graphics::par()].
#' @param ... Additional graphical parameters passed to [graphics::plot()].
#'
#' @return Invisibly returns a data frame containing the plotted dates, daily
#'   status values, HIS values, and CWIS values.
#'
#' @examples
#' \dontrun{
#' fit <- cwis(
#'   data = example_sp500_futures_spot_oneweek_24h_1sec,
#'   datetime_col = "datetime",
#'   futures_col = "V1",
#'   spot_col = "V2",
#'   tz = "America/New_York"
#' )
#'
#' plot(fit, market = "futures")
#' plot(fit, market = "spot")
#' }
#'
#' @export
plot.cwis_result <- function(
    x,
    y = NULL,
    market = c("futures", "spot"),
    his_colour = "blue",
    cwis_colour = "black",
    benchmark = 0.5,
    benchmark_colour = "grey50",
    line_width = 1.5,
    ylim = c(0, 1),
    xlab = "Trading date",
    ylab = "",
    main = NULL,
    date_format = "%Y-%m-%d",
    max_date_labels = 20L,
    show_legend = TRUE,
    legend_position = "topright",
    mar = c(8, 3, 0.5, 0.5),
    ...) {
  # The second plot argument is not used by this method.
  if (!is.null(y)) {
    stop(
      "`y` is not used; select a market with `market`.",
      call. = FALSE
    )
  }

  # Select either the futures or spot information-share series.
  market <- match.arg(market)

  # Require the result class returned by cwis().
  if (!inherits(x, "cwis_result")) {
    stop(
      "`x` must be a result returned by `cwis()`.",
      call. = FALSE
    )
  }

  daily <- x$daily
  his_column <- paste0("his_", market)
  cwis_column <- paste0("cwis_", market)
  required_columns <- c("date", "status", his_column, cwis_column)

  # Check that the daily table contains all plotting variables.
  if (!is.data.frame(daily) ||
      !all(required_columns %in% names(daily))) {
    stop(
      "`x$daily` does not contain the required plotting columns.",
      call. = FALSE
    )
  }

  if (nrow(daily) == 0L) {
    stop(
      "There are no daily results to plot.",
      call. = FALSE
    )
  }

  # Convert and sort dates before plotting.
  daily$date <- as.Date(daily$date)
  if (anyNA(daily$date)) {
    stop(
      "The daily result contains invalid dates.",
      call. = FALSE
    )
  }
  daily <- daily[order(daily$date), , drop = FALSE]

  # Retain only days for which the daily CWIS calculation succeeded.
  daily <- daily[!is.na(daily$status) & daily$status == "ok", , drop = FALSE]
  if (nrow(daily) == 0L) {
    stop(
      "There are no successful daily estimates to plot.",
      call. = FALSE
    )
  }

  dates <- daily$date
  his_values <- as.numeric(daily[[his_column]])
  cwis_values <- as.numeric(daily[[cwis_column]])

  # At least one successful estimate is required.
  if (!any(is.finite(his_values)) &&
      !any(is.finite(cwis_values))) {
    stop(
      "There are no successful daily estimates to plot.",
      call. = FALSE
    )
  }

  # Check the controls used to construct the date axis.
  if (!is.character(date_format) || length(date_format) != 1L ||
      is.na(date_format)) {
    stop(
      "`date_format` must be one character string.",
      call. = FALSE
    )
  }
  if (!is.numeric(max_date_labels) || length(max_date_labels) != 1L ||
      !is.finite(max_date_labels) || max_date_labels < 1L) {
    stop(
      "`max_date_labels` must be one positive number.",
      call. = FALSE
    )
  }
  max_date_labels <- as.integer(max_date_labels)

  # Use consecutive positions so failed dates do not leave blank spaces.
  plot_positions <- seq_along(dates)

  # Check the requested vertical plotting range.
  if (!is.numeric(ylim) || length(ylim) != 2L ||
      any(!is.finite(ylim)) || ylim[[1L]] >= ylim[[2L]]) {
    stop(
      "`ylim` must contain two increasing finite numbers.",
      call. = FALSE
    )
  }

  # Check the horizontal benchmark value.
  if (!is.numeric(benchmark) || length(benchmark) != 1L ||
      !is.finite(benchmark)) {
    stop(
      "`benchmark` must be one finite number.",
      call. = FALSE
    )
  }

  # Temporarily use margins that reproduce the original plotting layout.
  old_mar <- graphics::par("mar")
  on.exit(
    graphics::par(mar = old_mar),
    add = TRUE
  )
  graphics::par(mar = mar)

  # Create an empty plotting region for the successful trading days.
  graphics::plot(
    plot_positions,
    cwis_values,
    type = "n",
    xaxt = "n",
    xlab = "",
    ylab = ylab,
    ylim = ylim,
    main = main,
    ...
  )

  # Add the information-share benchmark.
  graphics::abline(
    h = benchmark,
    lty = "dashed",
    col = benchmark_colour
  )

  # Plot the daily Hasbrouck information-share midpoint.
  graphics::lines(
    plot_positions,
    his_values,
    col = his_colour,
    lwd = line_width
  )

  # Plot the daily contribution-weighted information share.
  graphics::lines(
    plot_positions,
    cwis_values,
    col = cwis_colour,
    lwd = line_width
  )

  # Select evenly spaced successful dates for the horizontal-axis labels.
  number_of_labels <- min(length(dates), max_date_labels)
  label_positions <- unique(as.integer(round(seq(
    from = 1L,
    to = length(dates),
    length.out = number_of_labels
  ))))

  # Display the actual dates of the selected successful observations.
  graphics::axis(
    side = 1,
    at = label_positions,
    labels = format(dates[label_positions], date_format),
    las = 2
  )

  # Place the axis title below the vertically oriented date labels.
  if (length(xlab) == 1L && !is.na(xlab) && nzchar(xlab)) {
    graphics::mtext(
      text = xlab,
      side = 1,
      line = 6
    )
  }

  if (isTRUE(show_legend)) {
    market_label <- if (market == "futures") "Futures" else "Spot"

    # Add a compact legend for HIS, CWIS, and the benchmark.
    graphics::legend(
      legend_position,
      legend = c(
        paste(market_label, "HIS"),
        paste(market_label, "CWIS"),
        paste0(format(100 * benchmark, trim = TRUE), "% benchmark")
      ),
      col = c(his_colour, cwis_colour, benchmark_colour),
      lty = c("solid", "solid", "dashed"),
      lwd = c(line_width, line_width, 1),
      bty = "n"
    )
  }

  # Invisibly return the plotted observations for further inspection.
  invisible(
    data.frame(
      date = dates,
      status = daily$status,
      his = his_values,
      cwis = cwis_values
    )
  )
}
