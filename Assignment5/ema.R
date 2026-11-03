# ema.R - Exponential Moving Average (base R only, no libraries)
# Author: Jaskaran Singh | BDA400 Assignment 5
#
# ema(data, period)
# Returns a vector the same length as `data`. The first value is the first data
# point; every later value is
#   EMA[i] = (data[i] - EMA[i-1]) * multiplier + EMA[i-1],  multiplier = 2 / (period + 1)

ema <- function(data, period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  if (length(period) != 1 || !is.numeric(period) || is.na(period) ||
      period < 1 || period %% 1 != 0) {
    stop("`period` must be a single positive whole number", call. = FALSE)
  }
  if (length(data) == 0) return(numeric(0))

  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Start from the first data point, then update with each later point.
  # Reduce(accumulate = TRUE) keeps every intermediate EMA value.
  ema_values <- Reduce(
    function(previous, price) (price - previous) * multiplier + previous,
    data[-1],
    accumulate = TRUE,
    init = data[1]
  )

  return(ema_values)
}
