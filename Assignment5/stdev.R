# stdev.R - Standard Deviation (base R only, no libraries)
# Author: Jaskaran Singh | BDA400 Assignment 5
#
# stdev(data)
# Returns the POPULATION standard deviation (divides by n, as in the assignment
# formula). Base R's sd() divides by n - 1, so stdev(x) = sd(x) * sqrt((n - 1) / n).

stdev <- function(data) {
  # Validate the input
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  n <- length(data)
  if (n == 0) stop("`data` must contain at least one value", call. = FALSE)

  # Calculate the mean of the data
  average <- sum(data) / n

  # Calculate the variance: the average squared distance from the mean
  variance <- sum((data - average)^2) / n

  # The standard deviation is the square root of the variance
  return(sqrt(variance))
}
