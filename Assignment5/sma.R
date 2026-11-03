# sma.R - Simple Moving Average (base R only, no libraries)
# Author: Jaskaran Singh | BDA400 Assignment 5
#
# sma(data, period)
#   data   : numeric vector
#   period : number of points in each window
# Returns length(data) - period + 1 values: the mean of every window of `period`
# consecutive points. Nothing is padded, as in the assignment pseudocode.

sma <- function(data, period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  if (length(period) != 1 || !is.numeric(period) || is.na(period) ||
      period < 1 || period %% 1 != 0) {
    stop("`period` must be a single positive whole number", call. = FALSE)
  }

  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period", call. = FALSE)
  }

  # Calculate SMA for each window of 'period' data points
  first_index <- seq_len(length(data) - period + 1)          # where each window starts
  sma_values  <- vapply(first_index, function(start) {
    window <- data[start:(start + period - 1)]               # the points in this window
    sum(window) / period                                     # their mean
  }, numeric(1))

  return(sma_values)
}
