# Generate the example price-path figure used in the package documentation.
#
# Run from the package root:
#
# source("tools/generate_example_price_plot.R")
#
# Outputs are saved in man/figures/.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(script_argument) == 1L) {
  script_path <- sub("^--file=", "", script_argument)
  package_root <- dirname(dirname(script_path))
} else {
  package_root <- "."
}

data_path <- file.path(
  package_root,
  "data",
  "example_sp500_futures_spot_oneweek_24h_1sec.rda"
)
output_directory <- file.path(package_root, "man", "figures")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  stop("Example data file not found: ", data_path, call. = FALSE)
}

data_environment <- new.env(parent = emptyenv())
loaded_objects <- load(data_path, envir = data_environment)
object_name <- "example_sp500_futures_spot_oneweek_24h_1sec"

if (!object_name %in% loaded_objects) {
  stop("Expected data object not found in the .rda file: ", object_name, call. = FALSE)
}

example_data <- get(object_name, envir = data_environment)
required_columns <- c("date", "time", "datetime", "V1", "V2")

if (!all(required_columns %in% names(example_data))) {
  stop(
    "Example data must contain: ",
    paste(required_columns, collapse = ", "),
    call. = FALSE
  )
}

# Use a complete mid-week trading day for a stable documentation example.
plot_date <- "2019-06-26"
day_data <- example_data[as.character(example_data$date) == plot_date, , drop = FALSE]

if (nrow(day_data) != 86400L) {
  stop("The selected date does not contain 86,400 one-second observations.", call. = FALSE)
}

time_parts <- strsplit(as.character(day_data$time), ":", fixed = TRUE)
seconds_from_midnight <- vapply(
  time_parts,
  function(parts) {
    as.numeric(parts[[1L]]) * 3600 +
      as.numeric(parts[[2L]]) * 60 +
      as.numeric(parts[[3L]])
  },
  numeric(1L)
)

# One point per minute is sufficient for display and keeps the SVG compact.
display_rows <- seconds_from_midnight %% 60 == 0
time_hours <- seconds_from_midnight[display_rows] / 3600
futures_price <- as.numeric(day_data$V1[display_rows])
spot_price_scaled <- 10 * as.numeric(day_data$V2[display_rows])

# Hide carried-forward observations when the corresponding market is closed.
futures_price[time_hours >= 17 & time_hours < 18] <- NA_real_
spot_price_scaled[time_hours < 4 | time_hours >= 20] <- NA_real_

colours <- c(
  futures = "#335F8A",
  spot = "#C8561A",
  futures_only = "#DCE8F3",
  overlap = "#DDEEDF",
  spot_only = "#F9E2D4",
  grid = "#D6DADE",
  boundary = "#8C96A1",
  text = "#1F2933",
  muted = "#5B6573"
)

draw_example_prices <- function() {
  old_parameters <- par(
    mar = c(5.3, 5.3, 5.1, 1.2),
    xaxs = "i",
    yaxs = "r",
    family = "sans",
    bg = "white"
  )
  on.exit(par(old_parameters), add = TRUE)

  y_range <- range(c(futures_price, spot_price_scaled), na.rm = TRUE)
  y_padding <- diff(y_range) * 0.07
  y_limits <- y_range + c(-y_padding, y_padding)

  plot(
    NA_real_,
    NA_real_,
    type = "n",
    xlim = c(0, 24),
    ylim = y_limits,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )

  periods <- data.frame(
    start = c(0, 4, 9.5, 16, 17, 18, 20),
    end = c(4, 9.5, 16, 17, 18, 20, 24),
    label = c("Pre", "Early", "Core", "Late I", "Maint.", "Late II", "Post"),
    fill = c(
      colours[["futures_only"]],
      colours[["overlap"]],
      colours[["overlap"]],
      colours[["overlap"]],
      colours[["spot_only"]],
      colours[["overlap"]],
      colours[["futures_only"]]
    ),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(periods))) {
    rect(
      periods$start[[i]],
      par("usr")[[3L]],
      periods$end[[i]],
      par("usr")[[4L]],
      col = periods$fill[[i]],
      border = NA
    )
  }

  y_ticks <- pretty(y_limits, n = 6)
  abline(h = y_ticks, col = colours[["grid"]], lwd = 0.8)

  boundaries <- c(0, 4, 9.5, 16, 17, 18, 20, 24)
  abline(v = boundaries, col = colours[["boundary"]], lty = 3, lwd = 0.8)

  lines(time_hours, futures_price, col = colours[["futures"]], lwd = 1.35)
  lines(time_hours, spot_price_scaled, col = colours[["spot"]], lwd = 1.25)

  axis(
    side = 1,
    at = boundaries,
    labels = c("00:00", "04:00", "09:30", "16:00", "17:00", "18:00", "20:00", "24:00"),
    tick = FALSE,
    line = 0.5,
    cex.axis = 0.84,
    col.axis = colours[["text"]]
  )
  axis(
    side = 2,
    at = y_ticks,
    las = 1,
    tck = -0.012,
    cex.axis = 0.86,
    col = colours[["boundary"]],
    col.axis = colours[["text"]]
  )
  box(col = colours[["boundary"]], lwd = 0.8)

  title(
    main = "S&P 500 futures and SPY prices on 26 June 2019",
    line = 3.2,
    cex.main = 1.35,
    font.main = 2,
    col.main = colours[["text"]]
  )
  mtext(
    "One-minute display sample from the one-second example data; SPY is multiplied by 10",
    side = 3,
    line = 1.85,
    cex = 0.82,
    col = colours[["muted"]]
  )
  mtext(
    "Time of day (ET)",
    side = 1,
    line = 3.3,
    cex = 0.92,
    col = colours[["text"]]
  )
  mtext(
    "Price level",
    side = 2,
    line = 3.6,
    cex = 0.92,
    col = colours[["text"]]
  )

  label_y <- par("usr")[[4L]] - 0.025 * diff(par("usr")[3:4])
  label_sizes <- c(0.68, 0.68, 0.68, 0.55, 0.52, 0.58, 0.68)
  for (i in seq_len(nrow(periods))) {
    text(
      (periods$start[[i]] + periods$end[[i]]) / 2,
      label_y,
      periods$label[[i]],
      cex = label_sizes[[i]],
      font = 2,
      col = colours[["text"]]
    )
  }

  legend(
    "bottomleft",
    inset = 0.02,
    legend = c("E-mini futures", "SPY x 10"),
    col = c(colours[["futures"]], colours[["spot"]]),
    lwd = c(2, 2),
    bty = "n",
    cex = 0.82,
    text.col = colours[["text"]]
  )

  invisible(NULL)
}

png_path <- file.path(output_directory, "example-sp500-price-paths.png")
svg_path <- file.path(output_directory, "example-sp500-price-paths.svg")

grDevices::png(
  filename = png_path,
  width = 2400,
  height = 1350,
  units = "px",
  res = 180,
  bg = "white"
)
draw_example_prices()
grDevices::dev.off()

grDevices::svg(
  filename = svg_path,
  width = 2400 / 180,
  height = 1350 / 180,
  bg = "white",
  pointsize = 12
)
draw_example_prices()
grDevices::dev.off()

message("Created: ", png_path)
message("Created: ", svg_path)
