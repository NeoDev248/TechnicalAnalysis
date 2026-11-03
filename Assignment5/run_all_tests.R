# run_all_tests.R - tests for all Assignment 5 indicator functions
# Author: Jaskaran Singh | BDA400 Assignment 5
#
# HOW TO RUN
#   1. Keep this file and the nine indicator scripts in one folder.
#   2. In RStudio: Session > Set Working Directory > To Source File Location.
#   3. Click Source (or run source("run_all_tests.R")).
# Expected values were calculated independently in Python from the assignment
# pseudocode. Base R functions (sd, filter) are used ONLY in this test file to
# cross-check the hand-written functions; the indicator scripts use no libraries.

for (f in c("sma", "ema", "macd", "stdev", "linreg", "rsi", "stoch_rsi",
            "crossover", "crossunder")) {
  source(paste0(f, ".R"))
}

passed <- 0; failed <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) passed <<- passed + 1 else failed <<- failed + 1
  cat(sprintf("%-62s %s\n", label, if (ok) "PASS" else "FAIL"))
}
close_to <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-8))
gives_error <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

# Template example data from the assignment, and my own 15-point price series
template <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
prices   <- c(32, 35, 31, 38, 41, 39, 44, 47, 43, 50, 48, 52, 49, 55, 53)

cat("\n--- SMA ---\n"); print(sma(template, 3))
check("sma: template example (period 3)",
      close_to(sma(template, 3), c(37/3, 47/3, 53/3, 20, 65/3, 71/3, 70/3)))
check("sma: my series, period 4",
      close_to(sma(prices, 4), c(34, 36.25, 37.25, 40.5, 42.75, 43.25, 46, 47, 48.25, 49.75, 51, 52.25)))
check("sma: agrees with stats::filter", close_to(sma(prices, 5),
      as.numeric(na.omit(stats::filter(prices, rep(1/5, 5), sides = 1)))))
check("sma: period 1 returns the data", close_to(sma(prices, 1), prices))
check("sma: period = length gives the mean", close_to(sma(prices, 15), mean(prices)))
check("sma: error if data shorter than period", gives_error(sma(c(1, 2), 3)))
check("sma: error for non-numeric data", gives_error(sma("a", 1)))

cat("\n--- EMA ---\n"); print(ema(template, 3))
check("ema: template example (period 3)",
      close_to(ema(template, 3), c(10, 11, 13, 16.5, 17.25, 19.625, 22.3125, 23.15625, 22.078125)))
check("ema: my series, period 5", close_to(ema(prices, 5),
      c(32, 33, 32.3333333333, 34.2222222222, 36.4814814815, 37.3209876543, 39.5473251029,
        42.0315500686, 42.3543667124, 44.9029111416, 45.9352740944, 47.9568493963,
        48.3045662642, 50.5363775095, 51.3575850063)))
check("ema: first value is the first data point", ema(prices, 5)[1] == prices[1])
check("ema: same length as the input", length(ema(prices, 5)) == length(prices))
check("ema: period 1 returns the data", close_to(ema(prices, 1), prices))
check("ema: error for period 0", gives_error(ema(prices, 0)))

cat("\n--- MACD ---\n")
m_ex <- macd(c(100, 105, 110, 115, 120, 125, 130), 3, 5, 2); print(m_ex$macd_line)
check("macd: template example, MACD line", close_to(m_ex$macd_line,
      c(0, 0.8333333333, 1.8055555556, 2.6620370370, 3.3371913580, 3.8393775720, 4.2002100137)))
m <- macd(prices, 3, 6, 4)
check("macd: my series, MACD line", close_to(m$macd_line,
      c(0, 0.6428571429, -0.0765306122, 1.1774781341, 2.0999843815, 1.7008817011, 2.3867905008,
        2.9336450005, 1.852715179, 2.7019952171, 2.1907387712, 2.5523273589, 1.6739908032,
        2.4068719186, 1.8962049)))
check("macd: my series, signal line", close_to(m$signal_line,
      c(0, 0.2571428571, 0.1236734694, 0.5451953353, 1.1671109538, 1.3806192527, 1.7830877519,
        2.2433106514, 2.0870724624, 2.3330415643, 2.276120447, 2.3866032118, 2.1015582483,
        2.2236837164, 2.0926921899)))
check("macd: histogram = MACD line - signal line", close_to(m$histogram, m$macd_line - m$signal_line))
check("macd: returns three named vectors", identical(names(m), c("macd_line", "signal_line", "histogram")))
check("macd: all vectors as long as the data", all(lengths(m) == length(prices)))

cat("\n--- Standard deviation ---\n"); print(stdev(template))
check("stdev: template data", close_to(stdev(template), 4.94662873101))
check("stdev: my series", close_to(stdev(prices), 7.3774430982))
check("stdev: equals sd() * sqrt((n-1)/n)", close_to(stdev(prices), sd(prices) * sqrt(14 / 15)))
check("stdev: constant data gives 0", stdev(c(4, 4, 4, 4)) == 0)
check("stdev: single value gives 0", stdev(7) == 0)
check("stdev: error for empty data", gives_error(stdev(numeric(0))))

cat("\n--- Linear regression ---\n")
l1 <- linreg(template, 5, 0); print(l1$slope); print(l1$intercept)
check("linreg: template data, slope", close_to(l1$slope, 0.7428571429))
check("linreg: template data, intercept", close_to(l1$intercept, 19.0666666667))
l2 <- linreg(prices, 6, 0)
check("linreg: my series (6, 0) slope and intercept", close_to(c(l2$slope, l2$intercept), c(1.4642857143, 44.1428571429)))
check("linreg: my series (6, 0) predicted values", close_to(l2$predicted_values,
      c(45.6071428571, 47.0714285714, 48.5357142857, 50, 51.4642857143, 52.9285714286, 54.3928571429)))
l3 <- linreg(prices, 8, 2)
check("linreg: my series (8, 2) predicted values", close_to(l3$predicted_values, c(45.6, 47, 48.4, 49.8, 51.2)))
# The pseudocode window for length 5, offset 0 on 10 points is elements 5 to 10,
# whose x values are 1..6, so the intercept is 11 and the first fitted value is 13.
check("linreg: perfect line has slope 2, intercept 11 (window quirk)",
      close_to(unname(unlist(linreg(2 * (1:10) + 3, 5, 0)[c("slope", "intercept")])), c(2, 11)))
check("linreg: error if length > data", gives_error(linreg(1:5, 6, 0)))
check("linreg: error if offset >= length", gives_error(linreg(1:5, 3, 3)))

cat("\n--- RSI ---\n"); print(rsi(prices, 5))
r <- rsi(prices, 5)
check("rsi: my series, period 5", close_to(r[6:15],
      c(60.4651162791, 71.0021321962, 75.8329631275, 59.3532684284, 72.449956548,
        64.9732178829, 72.156697004, 60.5205604206, 71.863809521, 64.1803417508)))
check("rsi: first `period` values are NA", all(is.na(r[1:5])) && !any(is.na(r[6:15])))
check("rsi: all values between 0 and 100", all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100))
check("rsi: only rises gives 100", all(rsi(1:12, 5)[6:12] == 100))
check("rsi: only falls gives 0", all(rsi(12:1, 5)[6:12] == 0))
check("rsi: flat prices give 50", all(rsi(rep(5, 12), 5)[6:12] == 50))
check("rsi: too little data gives NA with a warning",
      all(is.na(suppressWarnings(rsi(1:3, 5)))) && inherits(tryCatch(rsi(1:3, 5), warning = function(w) w), "warning"))

cat("\n--- Stochastic RSI ---\n")
s <- stoch_rsi(prices, 5, 3, 3); print(s$k_line)
check("stoch_rsi: %K line", close_to(s$k_line[6:13],
      c(0.5914431234, 0.5689538749, 0.5982388905, 0.3785797799, 0.6375536021, 0.3962587572,
        0.5356341474, 0.3742971122)))
check("stoch_rsi: %D line", close_to(s$d_line[6:11],
      c(0.5862119629, 0.5152575151, 0.5381240908, 0.4707973797, 0.5231488356, 0.4353966723)))
check("stoch_rsi: lengths are 13 and 11", length(s$k_line) == 13 && length(s$d_line) == 11)
check("stoch_rsi: values stay between 0 and 1",
      all(c(s$k_line, s$d_line)[!is.na(c(s$k_line, s$d_line))] >= 0 &
          c(s$k_line, s$d_line)[!is.na(c(s$k_line, s$d_line))] <= 1))
check("stoch_rsi: flat prices give all NA", all(is.na(unlist(stoch_rsi(rep(5, 20), 5, 3, 3)))))

cat("\n--- Crossover and crossunder ---\n")
a <- c(5, 7, 6, 9, 8, 6, 4, 5, 7, 9); b <- c(6, 6, 7, 7, 9, 7, 5, 4, 6, 8)
print(crossover(a, b)); print(crossunder(a, b))
check("crossover: my arrays", identical(crossover(a, b),
      c("None", "Up", "Down", "Up", "Down", "None", "None", "Up", "None", "None")))
check("crossunder: my arrays", identical(crossunder(a, b),
      c("None", "False", "True", "False", "True", "False", "False", "False", "False", "False")))
check("crossover: template arrays give one Up at position 4",
      identical(which(crossover(template, c(18, 20, 22, 18, 15, 12, 10, 11, 13)) == "Up"), 4L))
check("crossover: NA values give no signal", all(crossover(c(1, NA, 3), c(2, 2, 2)) == "None"))
check("crossunder: NA values give no signal", all(crossunder(c(1, NA, 3), c(2, 2, 2))[-1] == "False"))
check("crossover: equal arrays never cross", all(crossover(1:5, 1:5) == "None"))
check("crossover: error for different lengths", gives_error(crossover(1:3, 1:2)))
check("crossunder: error for different lengths", gives_error(crossunder(1:3, 1:2)))

cat(sprintf("\nTests passed: %d | failed: %d\n", passed, failed))
if (failed == 0) cat("All tests passed.\n") else stop("Some tests failed.")
