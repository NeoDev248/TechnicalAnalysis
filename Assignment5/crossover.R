# crossover.R - Crossover signals (base R only, no libraries)
# Author: Jaskaran Singh | BDA400 Assignment 5
#
# crossover(arr1, arr2)
# Returns a character vector the same length as the inputs:
#   "Up"   : arr1 crosses above arr2 here (arr1[i] > arr2[i] and arr1[i-1] <= arr2[i-1])
#   "Down" : arr1 crosses below arr2 here (arr1[i] < arr2[i] and arr1[i-1] >= arr2[i-1])
#   "None" : no crossing (always the case for the first element)
# Missing values never produce a signal.

crossover <- function(arr1, arr2) {
  # Check that both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length", call. = FALSE)
  }

  n <- length(arr1)
  signals <- rep("None", n)
  if (n < 2) return(signals)

  now  <- 2:n                                                   # current positions
  prev <- 1:(n - 1)                                             # previous positions

  went_up   <- arr1[now] > arr2[now] & arr1[prev] <= arr2[prev] # crossed above
  went_down <- arr1[now] < arr2[now] & arr1[prev] >= arr2[prev] # crossed below

  signals[now][which(went_up)]   <- "Up"                        # which() skips NA comparisons
  signals[now][which(went_down)] <- "Down"

  return(signals)
}
