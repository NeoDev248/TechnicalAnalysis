# stock_display.R
# Assignment 2 - utilities to display stock data, statistics and charts.
# Author: Jaskaran Singh. Needs stock_functions.R to be sourced first.

# Compact quote header, similar to the top of a Yahoo Finance page.
show_quote <- function(df, symbol) {
  n <- nrow(df)
  today <- df[n, ]
  prev  <- df$Close[n - 1]
  chg   <- today$Close - prev
  yr    <- df[df$Date >= today$Date - 365, ]

  cat("\n", symbol, " - ", format(today$Date), "\n", sep = "")
  cat(sprintf("  Last close : %.2f  (%+.2f, %+.2f%%)\n", today$Close, chg, 100 * chg / prev))
  cat(sprintf("  Open       : %.2f\n", today$Open))
  cat(sprintf("  Day range  : %.2f - %.2f\n", today$Low, today$High))
  cat(sprintf("  52wk range : %.2f - %.2f\n", min(yr$Low), max(yr$High)))
  cat(sprintf("  Volume     : %s\n", format(today$Volume, big.mark = ",")))
  invisible(df)
}

# Print head, tail and summary of the data frame.
show_table <- function(df, symbol, rows = 5) {
  cat("\n---", symbol, "---\n")
  cat(nrow(df), "trading days:", format(min(df$Date)), "to", format(max(df$Date)), "\n\n")
  cat("First rows\n"); print(head(df, rows), row.names = FALSE)
  cat("\nLast rows\n");  print(tail(df, rows), row.names = FALSE)
  cat("\nSummary\n");    print(summary(df[, -1]))
  invisible(df)
}

# Display every stock in the list.
show_all <- function(stocks, rows = 5) {
  for (s in names(stocks)) {
    show_quote(stocks[[s]], s)
    show_table(stocks[[s]], s, rows)
  }
  invisible(stocks)
}

# Line chart: Close price and its moving average.
chart_ma <- function(ma_df, symbol, window = 20) {
  lab <- paste0(window, "-day MA")
  ggplot(ma_df, aes(Date)) +
    geom_line(aes(y = Close, colour = "Close")) +
    geom_line(aes(y = MA, colour = lab), na.rm = TRUE) +
    scale_colour_manual(values = setNames(c("#444444", "#d95f02"), c("Close", lab)), name = NULL) +
    labs(title = paste(symbol, "close price and moving average"), x = NULL, y = "USD") +
    theme_minimal() + theme(legend.position = "bottom")
}

# Volume bars.
chart_volume <- function(df, symbol) {
  ggplot(df, aes(Date, Volume)) +
    geom_col(fill = "#1b7f79") +
    labs(title = paste(symbol, "daily volume"), x = NULL, y = "Shares") +
    theme_minimal()
}

# Compare all stocks with the first day set to 100.
chart_compare <- function(stocks) {
  d <- do.call(rbind, lapply(names(stocks), function(s) {
    x <- stocks[[s]]
    data.frame(Symbol = s, Date = x$Date, Index = x$Close / x$Close[1] * 100)
  }))
  ggplot(d, aes(Date, Index, colour = Symbol)) +
    geom_line() +
    labs(title = "Stocks compared (first day = 100)", x = NULL, y = "Index") +
    theme_minimal()
}

# Grouped bars of mean, median and mode.
chart_stats <- function(tbl) {
  d <- rbind(
    data.frame(Symbol = tbl$Symbol, Statistic = "Mean",   Value = tbl$Mean),
    data.frame(Symbol = tbl$Symbol, Statistic = "Median", Value = tbl$Median),
    data.frame(Symbol = tbl$Symbol, Statistic = "Mode",   Value = tbl$Mode)
  )
  ggplot(d, aes(Symbol, Value, fill = Statistic)) +
    geom_col(position = "dodge") +
    labs(title = "Mean, median and mode of close price", x = NULL, y = "USD") +
    theme_minimal()
}
