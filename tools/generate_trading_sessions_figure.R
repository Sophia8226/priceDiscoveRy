# Generate the trading-session diagram used in the package documentation.
#
# Run from the package root:
#
# source("tools/generate_trading_sessions_figure.R")
#
# Outputs are saved in man/figures/.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(script_argument) == 1L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_argument),
    winslash = "/",
    mustWork = TRUE
  )
  package_root <- dirname(dirname(script_path))
} else {
  package_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
}

output_directory <- file.path(package_root, "man", "figures")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

colours <- c(
  futures = "#4477AA",
  overlap = "#228833",
  spot = "#EE7733",
  closed = "#E6E8EB",
  text = "#1F2933",
  muted = "#5B6573",
  grid = "#AAB2BD"
)

add_label <- function(x, y, label, cex = 1, colour = colours[["text"]],
                      position = c("centre", "left", "right"), bold = FALSE) {
  position <- match.arg(position)
  adjustment <- switch(position, centre = 0.5, left = 0, right = 1)

  text(
    x,
    y,
    labels = label,
    adj = c(adjustment, 0.5),
    cex = cex,
    col = colour,
    font = if (bold) 2 else 1,
    xpd = NA
  )
}

add_bar <- function(start, end, y, height, fill, labels = character(),
                    label_cex = 0.9) {
  rect(
    start,
    y,
    end,
    y + height,
    col = fill,
    border = "white",
    lwd = 1.2
  )

  if (!length(labels)) {
    return(invisible(NULL))
  }

  label_colour <- if (identical(fill, colours[["closed"]])) {
    colours[["text"]]
  } else {
    "white"
  }

  centre_x <- (start + end) / 2
  centre_y <- y + height / 2

  if (length(labels) == 1L) {
    add_label(
      centre_x,
      centre_y,
      labels[[1L]],
      cex = label_cex,
      colour = label_colour,
      bold = TRUE
    )
  } else {
    add_label(
      centre_x,
      centre_y + 0.09,
      labels[[1L]],
      cex = label_cex,
      colour = label_colour,
      bold = TRUE
    )
    add_label(
      centre_x,
      centre_y - 0.11,
      labels[[2L]],
      cex = max(0.65, label_cex - 0.1),
      colour = label_colour
    )
  }

  invisible(NULL)
}

draw_trading_sessions <- function() {
  old_parameters <- par(
    mar = c(0, 0, 0, 0),
    xaxs = "i",
    yaxs = "i",
    family = "sans",
    bg = "white"
  )
  on.exit(par(old_parameters), add = TRUE)

  plot.new()
  plot.window(xlim = c(-2.7, 24.4), ylim = c(-1.35, 4.35))

  add_label(
    10.85,
    4.04,
    "Default 24-hour trading-session structure",
    cex = 1.65,
    bold = TRUE
  )
  add_label(
    10.85,
    3.75,
    "New York local time (ET); one-second observations",
    cex = 0.95,
    colour = colours[["muted"]]
  )

  add_label(-0.05, 3.33, "Market availability", cex = 1, position = "left", bold = TRUE)

  lane_height <- 0.44
  futures_y <- 2.68
  spot_y <- 1.98

  add_label(-0.35, futures_y + lane_height / 2, "E-mini futures", position = "right", bold = TRUE)
  add_label(-0.35, spot_y + lane_height / 2, "SPY ETF", position = "right", bold = TRUE)

  add_bar(0, 24, futures_y, lane_height, colours[["closed"]])
  add_bar(0, 17, futures_y, lane_height, colours[["futures"]], "Open")
  add_bar(17, 18, futures_y, lane_height, colours[["closed"]], "Closed", label_cex = 0.62)
  add_bar(18, 24, futures_y, lane_height, colours[["futures"]], "Open")

  add_bar(0, 24, spot_y, lane_height, colours[["closed"]])
  add_bar(4, 20, spot_y, lane_height, colours[["spot"]], "Open")

  add_label(
    -0.05,
    1.48,
    "Package session classification",
    cex = 1,
    position = "left",
    bold = TRUE
  )

  session_y <- 0.71
  session_height <- 0.55
  sessions <- list(
    list(0, 4, colours[["futures"]], c("Pre", "Futures only"), 0.82),
    list(4, 9.5, colours[["overlap"]], c("Early", "Both markets"), 0.82),
    list(9.5, 16, colours[["overlap"]], c("Core", "Both markets"), 0.82),
    list(16, 17, colours[["overlap"]], "Late I", 0.61),
    list(17, 18, colours[["spot"]], "Maint.", 0.59),
    list(18, 20, colours[["overlap"]], c("Late II", "Both"), 0.70),
    list(20, 24, colours[["futures"]], c("Post", "Futures only"), 0.82)
  )

  for (session in sessions) {
    add_bar(
      start = session[[1L]],
      end = session[[2L]],
      y = session_y,
      height = session_height,
      fill = session[[3L]],
      labels = session[[4L]],
      label_cex = session[[5L]]
    )
  }

  boundaries <- c(0, 4, 9.5, 16, 17, 18, 20, 24)
  boundary_labels <- c("00:00", "04:00", "09:30", "16:00", "17:00", "18:00", "20:00", "24:00")

  segments(
    boundaries,
    0.56,
    boundaries,
    3.19,
    col = colours[["grid"]],
    lty = 3,
    lwd = 0.8
  )
  text(
    boundaries,
    0.37,
    labels = boundary_labels,
    cex = 0.78,
    col = colours[["text"]],
    xpd = NA
  )
  add_label(12, 0.02, "Time of day (ET)", cex = 0.9)

  legend_items <- list(
    c("Futures only", colours[["futures"]]),
    c("Both markets open", colours[["overlap"]]),
    c("SPY only", colours[["spot"]]),
    c("Market closed", colours[["closed"]])
  )
  legend_positions <- c(3.1, 8.0, 13.9, 18.6)

  for (i in seq_along(legend_items)) {
    x <- legend_positions[[i]]
    rect(
      x,
      -0.55,
      x + 0.48,
      -0.40,
      col = legend_items[[i]][[2L]],
      border = NA,
      xpd = NA
    )
    add_label(
      x + 0.66,
      -0.475,
      legend_items[[i]][[1L]],
      cex = 0.76,
      position = "left"
    )
  }

  add_label(
    10.85,
    -1.08,
    "Current package convention: 16:00:00 is included in Core; Late I begins at 16:00:01.",
    cex = 0.75,
    colour = colours[["muted"]]
  )

  invisible(NULL)
}

png_path <- file.path(output_directory, "trading-sessions.png")
svg_path <- file.path(output_directory, "trading-sessions.svg")

grDevices::png(
  filename = png_path,
  width = 2400,
  height = 1080,
  units = "px",
  res = 144,
  bg = "white"
)
draw_trading_sessions()
grDevices::dev.off()

grDevices::svg(
  filename = svg_path,
  width = 2400 / 144,
  height = 1080 / 144,
  bg = "white",
  pointsize = 12
)
draw_trading_sessions()
grDevices::dev.off()

message("Created: ", png_path)
message("Created: ", svg_path)
