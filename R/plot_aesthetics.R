my_plot <- function(x, y, main = "My Custom Plot", type = "b", col = "#2b5c8f", pch = 16, ...) {
  # Create plot with cleaner aesthetics
  plot(x, y, main = main, type = type, col = col, pch = pch, bty = "n", las = 1, ...)

  # Add a subtle grid for better readability
  grid(col = "lightgray", lty = "dotted")
}
