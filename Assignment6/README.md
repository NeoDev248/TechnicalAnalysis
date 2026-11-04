# Assignment 6: Portfolio Dashboard (R Shiny)

BDA400 / Data Science Tools and Techniques. Technical Analysis using R, Visualization Phase.
Author: Jaskaran Singh

An interactive dashboard that downloads stock prices from Yahoo Finance, draws candlestick, line and area charts, overlays technical indicators and marks Buy / Sell / Hold signals from a trading rule.

## Run it

1. Open `app.R` in RStudio and click **Run App**, or run `shiny::runApp("app.R")` from this folder.
2. The first run installs any missing packages (`shiny`, `ggplot2`, `quantmod`).
3. Choose **Yahoo Finance** (needs internet) or **Demo data (offline)** in the top bar.

## What it does

| Assignment task | In the dashboard |
|-----------------|------------------|
| Data collection | Yahoo Finance through `quantmod`; offline demo data; clear error messages instead of crashes |
| Widgets | Data source, symbol, date range, time frame (Daily / Weekly / Monthly), chart type, indicator checkboxes and settings |
| Charts | Candlestick, line and area |
| Indicators | Moving averages (SMA or EMA) on the price chart, RSI and MACD panels, each switched on or off with a checkbox |
| Trading rules | MA Crossover, RSI Reversal and MACD Crossover, with adjustable settings |
| Annotations | Buy and Sell markers on the bars, optional Hold labels, a Signals tab and a Portfolio tab for every symbol in `portfolio.txt` |

The indicators (`sma`, `ema`, `macd`, `rsi`, `crossover`) are the base-R functions written in Assignment 5.

## Files

- `app.R`: the complete dashboard, in sections for Steps 1 to 4
- `portfolio.txt`: stock symbols from Assignment 2 (NVDA, JPM, KO, DIS, NFLX)
- `JaskaranSingh_BDA400_A06.docx`: cover page

*For learning only. This is not investment advice.*
