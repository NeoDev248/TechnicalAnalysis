# stoch_rsi.R - Stochastic RSI (base R only, no libraries)
# Author: Jaskaran Singh | BDA400 Assignment 5
#
# stoch_rsi(data, period, k_period, d_period)
# From the assignment pseudocode:
#   stoch   = (RSI - min RSI) / (max RSI - min RSI)   (min and max over all RSI values)
#   k_line  = sma(stoch, k_period)
#   d_line  = sma(k_line, d_period)
# Returns list(k_line, d_line). sma() does not pad, so k_line has
# length(data) - k_period + 1 values and d_line is shorter again. Values that
# depend on the NA warm-up of the RSI are NA.
# Needs rsi() and sma(); if they are not loaded, rsi.R and sma.R are sourced
# from the working directory (keep the files in the same folder).

if (!exists("sma", mode = "function")) source("sma.R")
if (!exists("rsi", mode = "function")) source("rsi.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Validate the input
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)

  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Rescale the RSI to 0..1 using its lowest and highest values
  known <- rsi_values[!is.na(rsi_values)]
  if (length(known) == 0 || max(known) == min(known)) {
    stoch <- rep(NA_real_, length(rsi_values))                  # nothing to rescale
  } else {
    stoch <- (rsi_values - min(known)) / (max(known) - min(known))
  }

  # Smooth into %K, then %D
  k_line <- sma(stoch, k_period)
  d_line <- sma(k_line, d_period)

  return(list(k_line = k_line, d_line = d_line))
}
