# stock_functions.R
# Assignment 2 - Technical Analysis using R (Preliminary Stage)
# Author: Jaskaran Singh
# Functions to import stock data (quantmod) and compute basic statistics.

pkgs_needed <- c("quantmod", "TTR", "ggplot2")

# Install any missing packages, then attach all of them.
setup_packages <- function(pkgs = pkgs_needed) {
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install)
  for (p in pkgs) library(p, character.only = TRUE)
  invisible(pkgs)
}

# ---- Import ----------------------------------------------------------------

# Read the ticker symbols (one per line) from portfolio.txt.
get_symbols_from_file <- function(path = "portfolio.txt") {
  if (!file.exists(path)) stop("File not found: ", path)
  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[nzchar(lines) & !grepl("^#", lines)]
  unique(toupper(lines))
}

# Convert an xts object from quantmod into a plain data frame.
xts_to_df <- function(x) {
  df <- data.frame(Date = as.Date(zoo::index(x)), zoo::coredata(x))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df <- df[complete.cases(df), ]
  row.names(df) <- NULL
  df
}

# Read portfolio.txt and download every symbol from Yahoo Finance.
# Returns a named list of data frames; each one is also saved in `env`
# under the symbol name (for example NVDA, JPM) as the assignment asks.
load_stock_data <- function(path = "portfolio.txt", from = "2024-01-01",
                            to = Sys.Date(), env = globalenv(), assign_vars = TRUE) {
  symbols <- get_symbols_from_file(path)
  out <- vector("list", length(symbols))
  names(out) <- symbols

  for (s in symbols) {
    raw <- try(getSymbols(s, src = "yahoo", from = from, to = to, auto.assign = FALSE),
               silent = TRUE)
    if (inherits(raw, "try-error")) {
      warning("Skipping ", s, ": download failed")
      next
    }
    out[[s]] <- xts_to_df(raw)
    if (assign_vars) assign(s, out[[s]], envir = env)
  }

  out <- Filter(Negate(is.null), out)
  if (!length(out)) stop("No data could be downloaded for any symbol.")
  out
}

# ---- Statistics ------------------------------------------------------------

# Mode = most frequent value after rounding (prices are continuous, so we round).
mode_value <- function(x, digits = 2) {
  x <- round(x[!is.na(x)], digits)
  tab <- table(x)
  as.numeric(names(tab)[tab == max(tab)][1])
}

# Statistics of the Close price for one stock.
# Returns list(summary = one-row data frame, ma = data frame with the moving average).
calculate_statistics <- function(df, window = 20) {
  px <- df$Close
  ma <- SMA(px, n = window)

  list(
    summary = data.frame(
      Days       = length(px),
      Mean       = mean(px),
      Median     = median(px),
      Mode       = mode_value(px),
      StdDev     = sd(px),
      Latest_MA  = tail(ma, 1),
      MA_Window  = window
    ),
    ma = data.frame(Date = df$Date, Close = px, MA = ma)
  )
}

# Run calculate_statistics() for every stock and combine the summaries.
calculate_all_statistics <- function(stocks, window = 20) {
  res <- lapply(stocks, calculate_statistics, window = window)
  tbl <- do.call(rbind, Map(function(r, s) cbind(Symbol = s, r$summary), res, names(res)))
  row.names(tbl) <- NULL
  list(by_stock = res, table = tbl)
}
