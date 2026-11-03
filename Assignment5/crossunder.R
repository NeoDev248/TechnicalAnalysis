# crossunder.R - Crossunder signals (base R only, no libraries)
# Author: Jaskaran Singh | BDA400 Assignment 5
#
# crossunder(arr1, arr2)
# Returns a character vector the same length as the inputs. The first element is
# "None". Every later element is "True" when arr1 crosses under arr2
# (arr1[i] < arr2[i] and arr1[i-1] >= arr2[i-1]) and "False" otherwise.
# Missing values never produce a signal.

crossunder <- function(arr1, arr2) {
  # Check that both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length", call. = FALSE)
  }

  n <- length(arr1)
  signals <- rep("None", n)
  if (n < 2) return(signals)

  now  <- 2:n                                                   # current positions
  prev <- 1:(n - 1)                                             # previous positions

  crossed <- arr1[now] < arr2[now] & arr1[prev] >= arr2[prev]   # crossed below
  signals[now] <- ifelse(!is.na(crossed) & crossed, "True", "False")

  return(signals)
}
