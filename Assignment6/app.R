# =============================================================================
# app.R - Portfolio Dashboard (R Shiny)
# BDA400 - Assignment 6: Technical Analysis using R, Visualization Phase
# Author: Jaskaran Singh
#
# An interactive dashboard that fetches stock data from Yahoo Finance, draws
# candlestick / line / area charts, overlays Moving Averages, RSI and MACD (the
# indicators written from scratch in Assignment 5), and annotates Buy / Sell /
# Hold signals from a trading rule chosen by the user.
#
# HOW TO RUN
#   1. Open this file in RStudio and click "Run App" (or shiny::runApp("app.R")).
#   2. The first run installs any missing packages (shiny, ggplot2, quantmod).
#   3. "Yahoo Finance" needs internet. "Demo data" works offline.
#
# SECTIONS (matching the assignment tasks)
#   Step 1  Data collection and setup
#   Step 2  Visualizing stock data (Shiny skeleton, widgets, charts)
#   Step 3  Overlay technical indicators (with on/off switches)
#   Step 4  Trading rules and annotations
# =============================================================================


# =============================================================================
# STEP 1: DATA COLLECTION AND SETUP
# =============================================================================

# ---- 1a. Install (if missing) and load the necessary packages ---------------
needed <- c("shiny", "ggplot2", "quantmod")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)

library(shiny)
library(ggplot2)
library(quantmod)     # used only to download Yahoo Finance data (getSymbols)

# ---- 1b. Indicators from Assignment 5 (base R, no libraries) -----------------
sma <- function(data, period) {
  if (length(data) < period) stop("Data length should be greater than or equal to the period")
  vapply(seq_len(length(data) - period + 1),
         function(start) sum(data[start:(start + period - 1)]) / period, numeric(1))
}

ema <- function(data, period) {
  multiplier <- 2 / (period + 1)
  Reduce(function(previous, price) (price - previous) * multiplier + previous,
         data[-1], accumulate = TRUE, init = data[1])
}

macd <- function(data, short_period, long_period, signal_period) {
  macd_line   <- ema(data, short_period) - ema(data, long_period)
  signal_line <- ema(macd_line, signal_period)
  list(macd_line = macd_line, signal_line = signal_line, histogram = macd_line - signal_line)
}

rsi <- function(data, period) {
  n <- length(data)
  if (n < period + 1) return(rep(NA_real_, n))
  change <- diff(data)
  gain <- ifelse(change > 0, change, 0)
  loss <- ifelse(change < 0, -change, 0)
  avg_gain <- sum(gain[1:period]) / period
  avg_loss <- sum(loss[1:period]) / period
  out <- rep(NA_real_, n)
  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gain[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + loss[i - 1]) / period
    out[i] <- if (avg_loss == 0 && avg_gain == 0) 50
              else if (avg_loss == 0) 100
              else 100 - 100 / (1 + avg_gain / avg_loss)
  }
  out
}

crossover <- function(arr1, arr2) {
  n <- length(arr1)
  signals <- rep("None", n)
  if (n < 2) return(signals)
  now <- 2:n; prev <- 1:(n - 1)
  signals[now][which(arr1[now] > arr2[now] & arr1[prev] <= arr2[prev])] <- "Up"
  signals[now][which(arr1[now] < arr2[now] & arr1[prev] >= arr2[prev])] <- "Down"
  signals
}

# sma() is shorter than its input, so pad the front with NA to line up with dates
pad_front <- function(values, total) c(rep(NA_real_, total - length(values)), values)
moving_average <- function(x, period, type = "SMA") {
  if (length(x) < period) return(rep(NA_real_, length(x)))
  if (type == "EMA") {
    out <- ema(x, period); out[seq_len(period - 1)] <- NA_real_; out   # skip the warm-up
  } else {
    pad_front(sma(x, period), length(x))
  }
}

# ---- 1c. Data sources ---------------------------------------------------------
HISTORY_DAYS <- 365 * 8                                    # history downloaded once per symbol

symbols_in_portfolio <- local({
  fallback <- c("NVDA", "JPM", "KO", "DIS", "NFLX")
  if (!file.exists("portfolio.txt")) return(fallback)
  s <- toupper(trimws(readLines("portfolio.txt", warn = FALSE)))
  s <- s[nzchar(s)]
  if (length(s) == 0) fallback else s
})

# Yahoo Finance through quantmod, with a readable error message
get_yahoo <- function(symbol, from, to) {
  xt <- tryCatch(
    suppressWarnings(getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE)),
    error = function(e) stop("Could not download '", symbol, "' from Yahoo Finance (",
                             conditionMessage(e), "). Check the symbol and your internet ",
                             "connection, or choose 'Demo data'.", call. = FALSE))
  df <- data.frame(Date = as.Date(index(xt)), Open = as.numeric(Op(xt)), High = as.numeric(Hi(xt)),
                   Low = as.numeric(Lo(xt)), Close = as.numeric(Cl(xt)), Volume = as.numeric(Vo(xt)))
  df[complete.cases(df), ]
}

# Synthetic prices (offline demo). The same symbol always gives the same series.
get_demo <- function(symbol, from, to) {
  days <- seq(as.Date(from), as.Date(to), by = "day")
  days <- days[as.integer(format(days, "%u")) <= 5]                # weekdays only
  seed <- sum(utf8ToInt(symbol)); set.seed(seed)
  n <- length(days)
  close <- (60 + 12 * (seed %% 11)) * exp(cumsum(rnorm(n, 0.0004, 0.016)))
  open  <- c(close[1], close[-n]) * exp(rnorm(n, 0, 0.004))
  data.frame(Date = days, Open = open,
             High = pmax(open, close) * (1 + abs(rnorm(n, 0, 0.006))),
             Low  = pmin(open, close) * (1 - abs(rnorm(n, 0, 0.006))),
             Close = close, Volume = round(runif(n, 1e7, 6e7)))
}

# Stop early if the data cannot be charted
check_data <- function(df) {
  if (is.null(df) || nrow(df) < 5) stop("Not enough price data was returned.", call. = FALSE)
  if (any(df$High < df$Low, na.rm = TRUE)) stop("Data problem: some High values are below Low.", call. = FALSE)
  df
}

# Convert daily bars to weekly or monthly bars
resample_bars <- function(df, time_frame) {
  if (time_frame == "Daily") return(df)
  bucket <- cut(df$Date, if (time_frame == "Weekly") "week" else "month")
  bars <- lapply(split(df, bucket), function(g) {
    if (nrow(g) == 0) return(NULL)
    data.frame(Date = max(g$Date), Open = g$Open[1], High = max(g$High),
               Low = min(g$Low), Close = g$Close[nrow(g)], Volume = sum(g$Volume))
  })
  out <- do.call(rbind, bars); rownames(out) <- NULL; out
}


# =============================================================================
# STEP 4 (helper): TRADING RULES - one "Buy", "Sell" or "Hold" per bar
# =============================================================================
make_signals <- function(df, rule, s) {
  n <- nrow(df)
  event <- switch(rule,
    "MA Crossover" = {
      # Buy when the short MA crosses above the long MA, Sell when it crosses below
      crossover(moving_average(df$Close, s$ma_short, s$ma_type),
                moving_average(df$Close, s$ma_long,  s$ma_type))
    },
    "RSI Reversal" = {
      # Buy when RSI climbs back above the oversold level, Sell when it drops below overbought
      r  <- rsi(df$Close, s$rsi_period)
      up <- crossover(r, rep(s$rsi_low, n))  == "Up"
      dn <- crossover(r, rep(s$rsi_high, n)) == "Down"
      ifelse(up, "Up", ifelse(dn, "Down", "None"))
    },
    "MACD Crossover" = {
      # Buy when the MACD line crosses above its signal line, Sell when it crosses below
      m <- macd(df$Close, s$macd_fast, s$macd_slow, s$macd_signal)
      e <- crossover(m$macd_line, m$signal_line)
      e[seq_len(min(s$macd_slow, n))] <- "None"                   # ignore the EMA warm-up
      e
    })
  ifelse(event == "Up", "Buy", ifelse(event == "Down", "Sell", "Hold"))
}


# =============================================================================
# STEP 2: SHINY SKELETON - USER INTERFACE (controls in a bar across the top)
# =============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f4f6f8; }
    .controls { background: #fff; border: 1px solid #dde2e7; border-radius: 6px;
                padding: 12px 15px 0 15px; margin-bottom: 12px; }
    .tile { display: inline-block; min-width: 125px; margin: 0 8px 10px 0; padding: 7px 13px;
            background: #fff; border: 1px solid #dde2e7; border-radius: 6px; }
    .tile .t { font-size: 11px; color: #667085; text-transform: uppercase; }
    .tile .v { font-size: 19px; font-weight: 600; }
    .pos { color: #1b7f4c; } .neg { color: #c0392b; }
  "))),
  titlePanel("Portfolio Dashboard"),

  # ---- interactive widgets ----
  div(class = "controls",
    fluidRow(
      column(2, selectInput("source", "Data source", c("Yahoo Finance", "Demo data (offline)"))),
      column(2, selectizeInput("symbol", "Stock symbol", choices = symbols_in_portfolio,
                               selected = symbols_in_portfolio[1], options = list(create = TRUE))),
      column(3, dateRangeInput("date_range", "Select Date Range:",
                               start = Sys.Date() - 365, end = Sys.Date())),
      column(2, selectInput("time_frame", "Select Time Frame:", c("Daily", "Weekly", "Monthly"))),
      column(2, selectInput("chart_type", "Chart type", c("Candlestick", "Line", "Area")))
    ),
    fluidRow(
      column(3, checkboxGroupInput("indicators", "Overlay indicators", inline = TRUE,
                                   choices = c("Moving Averages", "RSI", "MACD"),
                                   selected = "Moving Averages")),
      column(3, selectInput("rule", "Trading rule",
                            c("MA Crossover", "RSI Reversal", "MACD Crossover"))),
      column(3, checkboxInput("show_signals", "Annotate Buy / Sell on the chart", TRUE),
                checkboxInput("show_hold", "Also label Hold bars", FALSE)),
      column(3, selectInput("ma_type", "Moving average type", c("SMA", "EMA")))
    ),
    # indicator settings only appear when they are used
    conditionalPanel("input.indicators.indexOf('Moving Averages') > -1 || input.rule == 'MA Crossover'",
      fluidRow(column(2, numericInput("ma_short", "Short MA", 20, min = 2, max = 200)),
               column(2, numericInput("ma_long", "Long MA", 50, min = 3, max = 400)))),
    conditionalPanel("input.indicators.indexOf('RSI') > -1 || input.rule == 'RSI Reversal'",
      fluidRow(column(2, numericInput("rsi_period", "RSI period", 14, min = 2, max = 100)),
               column(2, numericInput("rsi_low", "Oversold", 30, min = 1, max = 49)),
               column(2, numericInput("rsi_high", "Overbought", 70, min = 51, max = 99)))),
    conditionalPanel("input.indicators.indexOf('MACD') > -1 || input.rule == 'MACD Crossover'",
      fluidRow(column(2, numericInput("macd_fast", "MACD fast", 12, min = 2, max = 100)),
               column(2, numericInput("macd_slow", "MACD slow", 26, min = 3, max = 200)),
               column(2, numericInput("macd_signal", "MACD signal", 9, min = 2, max = 100))))
  ),

  uiOutput("tiles"),
  tabsetPanel(
    tabPanel("Chart", plotOutput("price_plot", height = "auto")),
    tabPanel("Signals", br(), uiOutput("signal_counts"), br(), tableOutput("signal_table")),
    tabPanel("Portfolio", br(),
             p("Latest close, 1-day change and latest signal for every symbol in portfolio.txt."),
             actionButton("load_portfolio", "Load portfolio overview"), br(), br(),
             tableOutput("portfolio_table")),
    tabPanel("Data", br(), tableOutput("data_table")),
    tabPanel("About", br(),
      p("Data: Yahoo Finance through quantmod, or synthetic demo data when offline."),
      p("Indicators (SMA, EMA, MACD, RSI, crossover) are the base-R functions from Assignment 5."),
      p("Buy when the rule's crossover turns up, Sell when it turns down, otherwise Hold."),
      p(strong("For learning only. This is not investment advice.")))
  )
)


# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  cache <- new.env()                                   # downloaded data is kept per symbol

  load_symbol <- function(sym, source) {
    key <- paste(source, sym, sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])
    to <- Sys.Date(); from <- to - HISTORY_DAYS
    df <- if (source == "Yahoo Finance") get_yahoo(sym, from, to) else get_demo(sym, from, to)
    cache[[key]] <- check_data(df)
    cache[[key]]
  }

  # ---- Step 1: fetch the data ---------------------------------------------------
  raw_data <- reactive({
    sym <- toupper(trimws(input$symbol))
    validate(need(nzchar(sym), "Enter a stock symbol."))
    df <- tryCatch(
      withProgress(message = paste("Loading", sym, "..."), value = 0.5, load_symbol(sym, input$source)),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 12)
        NULL
      })
    validate(need(!is.null(df), "No data to show. See the error message, or try 'Demo data'."))
    df
  })

  # ---- Step 2: filter by date range and time frame ------------------------------
  prices <- reactive({
    df <- raw_data()
    validate(need(input$date_range[1] < input$date_range[2], "Start date must be before the end date."))
    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(df) >= 5, "Fewer than 5 trading days in this range. Widen the date range."))
    resample_bars(df, input$time_frame)
  })

  settings <- reactive(list(
    ma_short = input$ma_short, ma_long = input$ma_long, ma_type = input$ma_type,
    rsi_period = input$rsi_period, rsi_low = input$rsi_low, rsi_high = input$rsi_high,
    macd_fast = input$macd_fast, macd_slow = input$macd_slow, macd_signal = input$macd_signal))

  # ---- Steps 3 and 4: add indicators and trading signals ------------------------
  analysed <- reactive({
    df <- prices(); s <- settings()
    validate(
      need(s$ma_short < s$ma_long, "Short MA must be smaller than long MA."),
      need(s$macd_fast < s$macd_slow, "MACD fast period must be smaller than slow."),
      need(s$rsi_low < s$rsi_high, "RSI oversold level must be below overbought."))
    df$MA_short <- moving_average(df$Close, s$ma_short, s$ma_type)
    df$MA_long  <- moving_average(df$Close, s$ma_long,  s$ma_type)
    df$RSI      <- rsi(df$Close, s$rsi_period)
    m <- macd(df$Close, s$macd_fast, s$macd_slow, s$macd_signal)
    df$MACD <- m$macd_line; df$MACD_signal <- m$signal_line; df$MACD_hist <- m$histogram
    df$Signal <- make_signals(df, input$rule, s)
    df
  })

  # ---- summary tiles ------------------------------------------------------------
  output$tiles <- renderUI({
    df <- analysed(); n <- nrow(df)
    change <- if (n > 1) 100 * (df$Close[n] / df$Close[n - 1] - 1) else 0
    last_signal <- tail(df$Signal[df$Signal != "Hold"], 1)
    if (length(last_signal) == 0) last_signal <- "None yet"
    tile <- function(label, value, cls = "")
      div(class = "tile", div(class = "t", label), div(class = paste("v", cls), value))
    tagList(
      tile("Symbol", toupper(trimws(input$symbol))),
      tile("Last close", sprintf("$%.2f", df$Close[n])),
      tile("Change", sprintf("%+.2f%%", change), if (change >= 0) "pos" else "neg"),
      tile("High", sprintf("$%.2f", max(df$High))),
      tile("Low", sprintf("$%.2f", min(df$Low))),
      tile("Latest signal", last_signal,
           if (last_signal == "Buy") "pos" else if (last_signal == "Sell") "neg" else ""))
  })

  # ---- chart panels (each panel is a small function) ----------------------------
  bar_width <- function() switch(input$time_frame, Daily = 0.35, Weekly = 2, Monthly = 9)

  price_panel <- function(df) {
    df$direction <- ifelse(df$Close >= df$Open, "Up", "Down")
    colours <- c(Up = "#1b7f4c", Down = "#c0392b", "Short MA" = "#2563eb", "Long MA" = "#f59e0b")
    p <- ggplot(df, aes(x = Date))

    # Step 2: the chart type chosen by the user
    if (input$chart_type == "Candlestick") {
      w <- bar_width()
      p <- p +
        geom_segment(aes(xend = Date, y = Low, yend = High, colour = direction)) +
        geom_rect(aes(xmin = Date - w, xmax = Date + w, ymin = pmin(Open, Close),
                      ymax = pmax(Open, Close), fill = direction)) +
        scale_fill_manual(values = colours, guide = "none")
    } else if (input$chart_type == "Area") {
      p <- p + geom_ribbon(aes(ymin = min(Close) * 0.98, ymax = Close), fill = "#bfdbfe", alpha = 0.7) +
               geom_line(aes(y = Close), colour = "#1d4ed8", linewidth = 0.7)
    } else {
      p <- p + geom_line(aes(y = Close), colour = "#1d4ed8", linewidth = 0.7)
    }

    # Step 3: moving averages overlaid on the price chart
    breaks <- character(0)
    if ("Moving Averages" %in% input$indicators) {
      p <- p + geom_line(aes(y = MA_short, colour = "Short MA"), na.rm = TRUE, linewidth = 0.8) +
               geom_line(aes(y = MA_long,  colour = "Long MA"),  na.rm = TRUE, linewidth = 0.8)
      breaks <- c("Short MA", "Long MA")
    }

    # Step 4: annotate the bars with the signals
    if (isTRUE(input$show_signals)) {
      span <- diff(range(c(df$Low, df$High)))
      buys <- df[df$Signal == "Buy", ]; sells <- df[df$Signal == "Sell", ]
      if (nrow(buys) > 0) p <- p +
        geom_point(data = buys, aes(y = Low - 0.06 * span), shape = 24, size = 3.5,
                   fill = "#1b7f4c", colour = "#14532d") +
        geom_text(data = buys, aes(y = Low - 0.125 * span, label = "Buy"),
                  colour = "#14532d", fontface = "bold", size = 3.4)
      if (nrow(sells) > 0) p <- p +
        geom_point(data = sells, aes(y = High + 0.06 * span), shape = 25, size = 3.5,
                   fill = "#c0392b", colour = "#7f1d1d") +
        geom_text(data = sells, aes(y = High + 0.125 * span, label = "Sell"),
                  colour = "#7f1d1d", fontface = "bold", size = 3.4)
      if (isTRUE(input$show_hold)) {
        holds <- df[df$Signal == "Hold", ]
        p <- p + geom_text(data = holds, aes(y = Close, label = "Hold"), colour = "grey50",
                           size = 2.3, vjust = 1.8, check_overlap = TRUE)
      }
    }

    p + scale_colour_manual(values = colours, breaks = breaks, name = NULL) +
      scale_y_continuous(labels = function(v) paste0("$", format(v, big.mark = ","))) +
      labs(title = paste0(toupper(trimws(input$symbol)), ": ", input$chart_type, " chart (",
                          tolower(input$time_frame), ")"),
           subtitle = paste("Trading rule:", input$rule), x = NULL, y = "Price") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))
  }

  rsi_panel <- function(df) {
    lo <- input$rsi_low; hi <- input$rsi_high
    ggplot(df, aes(Date, RSI)) +
      annotate("rect", xmin = min(df$Date), xmax = max(df$Date), ymin = hi, ymax = 100, fill = "#c0392b", alpha = 0.10) +
      annotate("rect", xmin = min(df$Date), xmax = max(df$Date), ymin = 0, ymax = lo, fill = "#1b7f4c", alpha = 0.10) +
      geom_hline(yintercept = c(lo, hi), linetype = "dashed", colour = "grey55") +
      geom_line(colour = "#7c3aed", na.rm = TRUE, linewidth = 0.8) +
      scale_y_continuous(limits = c(0, 100), breaks = c(0, lo, 50, hi, 100)) +
      labs(x = NULL, y = paste0("RSI (", input$rsi_period, ")")) + theme_minimal(base_size = 13)
  }

  macd_panel <- function(df) {
    df$sign <- ifelse(df$MACD_hist >= 0, "pos", "neg")
    ggplot(df, aes(Date)) +
      geom_col(aes(y = MACD_hist, fill = sign), width = bar_width() * 2, alpha = 0.55, position = "identity") +
      scale_fill_manual(values = c(pos = "#1b7f4c", neg = "#c0392b"), guide = "none") +
      geom_line(aes(y = MACD), colour = "#2563eb", linewidth = 0.8) +
      geom_line(aes(y = MACD_signal), colour = "#f59e0b", linewidth = 0.8) +
      geom_hline(yintercept = 0, colour = "grey50") +
      labs(x = NULL, y = "MACD") + theme_minimal(base_size = 13)
  }

  # Stack the panels so their date axes line up (the price panel is the tallest)
  stack_panels <- function(panels, xlim) {
    panels <- lapply(panels, function(p) p + coord_cartesian(xlim = xlim))
    if (length(panels) == 1) return(print(panels[[1]]))
    for (i in seq_len(length(panels) - 1)) panels[[i]] <- panels[[i]] + theme(axis.text.x = element_blank())
    grobs   <- lapply(panels, ggplotGrob)
    stacked <- do.call(rbind, c(grobs, list(size = "max")))
    rows    <- unique(stacked$layout$t[grepl("^panel", stacked$layout$name)])
    stacked$heights[rows] <- grid::unit(c(4, 1.5, 1.7)[seq_along(grobs)], "null")
    grid::grid.newpage(); grid::grid.draw(stacked)
  }

  # ---- Steps 2 and 3: draw the chart with the selected overlays -----------------
  output$price_plot <- renderPlot({
    df <- analysed(); on <- input$indicators
    panels <- list(price_panel(df))
    if ("RSI" %in% on)  panels[[length(panels) + 1]] <- rsi_panel(df)
    if ("MACD" %in% on) panels[[length(panels) + 1]] <- macd_panel(df)
    stack_panels(panels, range(df$Date))
  }, height = function() 430 + 170 * ("RSI" %in% input$indicators) + 190 * ("MACD" %in% input$indicators),
  res = 96)

  # ---- Step 4: signals tab --------------------------------------------------------
  output$signal_counts <- renderUI({
    counts <- table(factor(analysed()$Signal, levels = c("Buy", "Sell", "Hold")))
    div(strong("Rule: "), input$rule, br(), strong("Signals in this period: "),
        span(class = "pos", paste(counts["Buy"], "Buy")), " | ",
        span(class = "neg", paste(counts["Sell"], "Sell")), " | ", span(paste(counts["Hold"], "Hold")))
  })

  output$signal_table <- renderTable({
    df <- analysed(); s <- df[df$Signal != "Hold", c("Date", "Signal", "Close")]
    validate(need(nrow(s) > 0, "No Buy/Sell signals with these settings. Try other parameters or a longer date range."))
    s$Date <- format(s$Date); s$Close <- sprintf("$%.2f", s$Close)
    s[order(s$Date, decreasing = TRUE), ]
  }, striped = TRUE, spacing = "s")

  # ---- Portfolio tab: one row per symbol in portfolio.txt ------------------------
  portfolio_overview <- eventReactive(input$load_portfolio, {
    s <- settings()
    rows <- lapply(symbols_in_portfolio, function(sym) {
      df <- tryCatch(load_symbol(sym, input$source), error = function(e) NULL)
      if (is.null(df)) return(data.frame(Symbol = sym, Close = NA, Change = NA, Signal = "No data"))
      n <- nrow(df); sig <- make_signals(df, input$rule, s); last <- tail(sig[sig != "Hold"], 1)
      data.frame(Symbol = sym, Close = df$Close[n], Change = 100 * (df$Close[n] / df$Close[n - 1] - 1),
                 Signal = if (length(last)) last else "None yet")
    })
    out <- do.call(rbind, rows)
    out$Close <- ifelse(is.na(out$Close), "", sprintf("$%.2f", out$Close))
    out$Change <- ifelse(is.na(out$Change), "", sprintf("%+.2f%%", out$Change))
    names(out)[4] <- "Latest signal"
    out
  })
  output$portfolio_table <- renderTable(portfolio_overview(), striped = TRUE, spacing = "s")

  output$data_table <- renderTable({
    out <- tail(analysed()[, c("Date", "Open", "High", "Low", "Close", "Volume", "Signal")], 50)
    out$Date <- format(out$Date)
    out$Volume <- format(round(out$Volume), big.mark = ",", trim = TRUE)
    out[order(out$Date, decreasing = TRUE), ]
  }, striped = TRUE, spacing = "s", digits = 2)
}

# =============================================================================
# Launch the app
# =============================================================================
shinyApp(ui, server)
