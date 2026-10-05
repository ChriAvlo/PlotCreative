plot_type_choice <- function(x, y = NULL, ...) {
  # Case 1: Single variable provided
  if (is.null(y)) {
    if (is.numeric(x)) {
      hist(x, main = "Histogram (Numeric Data)", col = "#2b5c8f", border = "white", ...)
    } else if (is.factor(x) || is.character(x) || is.logical(x)) {
      barplot(table(x), main = "Bar Chart (Categorical Data)", col = "#2b5c8f", ...)
    } else {
      stop("Unsupported data type for a single variable.")
    }
  }
  # Case 2: Two variables provided
  else {
    # Numeric vs Numeric -> Scatter plot
    if (is.numeric(x) && is.numeric(y)) {
      plot(x, y, main = "Scatter Plot (Numeric vs Numeric)", pch = 16, col = "#2b5c8f", ...)
    }
    # Categorical/Character vs Numeric -> Box plot
    else if ((is.factor(x) || is.character(x)) && is.numeric(y)) {
      boxplot(y ~ x, main = "Box Plot (Categorical vs Numeric)", col = "#2b5c8f", xlab = "Groups", ylab = "Values", ...)
    }
    # Categorical vs Categorical -> Grouped Bar Chart
    else if ((is.factor(x) || is.character(x)) && (is.factor(y) || is.character(y))) {
      barplot(table(x, y), beside = TRUE, legend.text = TRUE,
              main = "Grouped Bar Chart (Categorical vs Categorical)", ...)
    }
    # Fallback default
    else {
      plot(x, y, main = "Custom Relationship Plot", pch = 16, col = "#2b5c8f", ...)
    }
  }
}
