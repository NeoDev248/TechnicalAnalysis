# rsi.R - Relative Strength Index (base R only, no libraries)
# Author: Jaskaran Singh | BDA400 Assignment 5
#
# rsi(data, period)
# Returns a vector the same length as `data`. The first `period` values are NA
# (not enough history). Later values are 100 - 100 / (1 + RS), where RS is the
# Wilder-smoothed average gain divided by the average loss. As in the pseudocode,
# the smoothing starts at position period + 1.
# Special cases: no losses -> 100; no gains and no losses -> 50.
# With fewer than period + 1 points a vector of NA is returned with a warning.

rsi <- function(data, period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  if (length(period) != 1 || !is.numeric(period) || is.na(period) ||
      period < 1 || period %% 1 != 0) {
    stop("`period` must be a single positive whole number", call. = FALSE)
  }

  n <- length(data)
  if (n < period + 1) {
    warning("Need at least period + 1 data points; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }

  # Price changes, split into gains (rises) and losses (falls, as positive numbers)
  change <- diff(data)
  gain <- ifelse(change > 0, change, 0)
  loss <- ifelse(change < 0, -change, 0)

  # Average gain and loss over the first `period` changes
  avg_gain <- sum(gain[1:period]) / period
  avg_loss <- sum(loss[1:period]) / period

  # Wilder smoothing from position period + 1 onward
  rsi_values <- rep(NA_real_, n)
  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gain[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + loss[i - 1]) / period

    rsi_values[i] <-
      if (avg_loss == 0 && avg_gain == 0) 50                   # flat prices
      else if (avg_loss == 0) 100                              # only gains
      else 100 - 100 / (1 + avg_gain / avg_loss)               # 100 - 100 / (1 + RS)
  }

  return(rsi_values)
}
