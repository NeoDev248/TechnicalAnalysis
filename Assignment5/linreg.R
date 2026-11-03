# linreg.R - Simple Linear Regression (base R only, no libraries)
# Author: Jaskaran Singh | BDA400 Assignment 5
#
# linreg(regressionSource, regressionLength, regressionOffset)
#   regressionSource : numeric vector of data points
#   regressionLength : how many points back the regression looks
#   regressionOffset : points trimmed from the end (0 = regress up to the last point)
# Window (from the assignment pseudocode):
#   start_index = max(1, n - regressionLength + regressionOffset)
#   end_index   = min(n, n - regressionOffset)
# The x values are 1, 2, 3, ... inside the window.
# Returns list(slope, intercept, predicted_values).

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Validate the inputs
  if (!is.numeric(regressionSource)) {
    stop("`regressionSource` must be a numeric vector", call. = FALSE)
  }
  if (length(regressionLength) != 1 || length(regressionOffset) != 1 ||
      !is.numeric(regressionLength) || !is.numeric(regressionOffset) ||
      regressionOffset < 0) {
    stop("`regressionLength` and `regressionOffset` must be single numbers; offset >= 0",
         call. = FALSE)
  }

  n <- length(regressionSource)                                 # number of data points
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource",
         call. = FALSE)
  }
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength", call. = FALSE)
  }

  # Pick the window of points to use
  first <- max(1, n - regressionLength + regressionOffset)
  last  <- min(n, n - regressionOffset)
  y <- regressionSource[first:last]                             # the values
  if (length(y) < 2) stop("At least two data points are needed for a regression", call. = FALSE)
  x <- seq_along(y)                                             # 1, 2, 3, ...

  # Least squares: slope = covariance(x, y) / variance(x)
  x_bar <- sum(x) / length(x)
  y_bar <- sum(y) / length(y)
  slope     <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)
  intercept <- y_bar - slope * x_bar

  # Fitted values on the regression line
  predicted_values <- intercept + slope * x

  return(list(slope = slope, intercept = intercept, predicted_values = predicted_values))
}
