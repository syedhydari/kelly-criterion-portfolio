library(tidyverse)
library(tidyquant)
library(scales)
library(quantmod)
library(xts)
library(tibble)
library(fpp3)
library(zoo)
library(PerformanceAnalytics)
library(fPortfolio)
library(kableExtra)
library(moments)
library(portfolioBacktest)
library(quadprog)


# Dow Jones Kelly Strategies Test


# Create output directory for figures
output_dir <- "output/figures/DJI"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat(sprintf("Created output directory: %s\n", output_dir))
}

# DOWNLOAD AND PREPARE DATA

# Download Dow Jones Industrial Average Data
ticker_dj <- "^DJI"
start_date_dj <- "2007-01-01"
end_date_dj <- "2018-12-31"

# Download data from Yahoo Finance for testing
prices_dj_xts <- getSymbols(ticker_dj, 
                            from = start_date_dj,
                            to = end_date_dj,
                            auto.assign = FALSE,
                            warnings = FALSE)

prices_dj <- Ad(prices_dj_xts)
returns_dj <- diff(log(prices_dj))[-1]
colnames(returns_dj) <- "DJI"

prices <- tq_get(ticker_dj,
  get = "stock.prices",
  from = start_date_dj,
  to = end_date_dj)

prices %>%
  select(symbol, date, adjusted) |>
  pivot_wider(names_from = symbol,
  values_from = adjusted) -> port_data

# print(port_data)

# port_data <- xts(x = port_data[, -1], order.by = port_data$date)
port_data <- xts(x = port_data[, -which(names(port_data) == "date")],
                  order.by = port_data$date)
port_data <- list(adjusted = port_data)

print(port_data)

# Risk-free rate
rf_annual <- 0.01 # 0.01
rf_daily <- rf_annual / 252

cat("\n=== Data Downloaded ===\n")
cat(sprintf("Ticker: %s\n", ticker_dj))
cat(sprintf("Period: %s to %s\n", start_date_dj, end_date_dj))
cat(sprintf("Number of observations: %d\n", nrow(returns_dj)))

# Kelly Opt Functions for Portfolio Backtesting

# Safe Kelly computation for single time series
safe_kelly_fraction <- function(x, rf) {
  mu <- mean(x, na.rm = TRUE)
  sigma2 <- var(x, na.rm = TRUE)

  if (!is.finite(mu) || !is.finite(sigma2) || sigma2 <= 0) {
    return(0)
  }

  f <- (mu - rf) / sigma2

  if (!is.finite(f)) f <- 0
  return(f)
}

# Compute weights for multiplier m = 1, .5, 2, 3
kelly_strategy_factory <- function(multiplier) {
  function(dataset, ...) {
    ret <- diff(log(dataset$adjusted))[-1]

    # Portfolio return = equal-weighted proxy for Kelly
    port_ret <- rowMeans(ret, na.rm = TRUE)

    f <- safe_kelly_fraction(port_ret, rf_daily) * multiplier

    # Cap leverage to prevent NaN/Inf weight vectors
    f <- max(min(f, 3), -3)

    # Equal weights scaled by f
    w <- rep(1 / ncol(ret), ncol(ret)) * f

    # Normalize if backtester requires weights sum to 1
    if (all(w == 0) || any(!is.finite(w))) {
      w <- rep(1 / ncol(ret), ncol(ret))   # fallback to equal weight
    }

    names(w) <- colnames(ret)
    return(w)
  }
}

# safe_kelly_fraction <- function(x, rf) {
#   mu <- mean(x, na.rm = TRUE)
#   sigma2 <- var(x, na.rm = TRUE)

#   if (!is.finite(mu) || !is.finite(sigma2) || sigma2 <= 0) {
#     return(0)
#   }

#   f <- (mu - rf) / sigma2

#   if (!is.finite(f)) f <- 0
#   return(f)
# }

# Long-only single-asset Kelly with multiplier
kelly_long_only_factory <- function(multiplier = 1) {
  function(dataset, ...) {

    # Single asset log returns
    ret <- diff(log(dataset$adjusted))[-1]

    # Original Kelly fraction
    f <- safe_kelly_fraction(ret, rf_daily)

    # Apply multiplier
    f <- f * multiplier

    # Enforce long-only constraints: 0 ≤ f ≤ 1
    f <- max(min(f, 1), 0)

    # Return single weight vector
    # portfolioBacktest expects a named numeric vector
    w <- c(asset = f)

    return(w)
  }
}

# Final strategies
kelly_full_long   <- kelly_long_only_factory(1)
kelly_half_long   <- kelly_long_only_factory(0.5)
kelly_full    <- kelly_strategy_factory(1)
kelly_half    <- kelly_strategy_factory(0.5)
kelly_double  <- kelly_strategy_factory(2)
kelly_triple  <- kelly_strategy_factory(3)



# 5. Buy and Hold (100% in risky asset)
buy_and_hold <- function(dataset, ...) {
  prices <- dataset$adjusted
  w <- rep(1.0, ncol(prices))
  names(w) <- colnames(prices)
  
  return(w)
}



# BackTesting Portfolio List

portfolio_list <- list(
  "Buy & Hold"   = buy_and_hold,
  "Half Kelly"   = kelly_half,
  "Full Kelly"   = kelly_full,
  "Double Kelly" = kelly_double,
  "Triple Kelly" = kelly_triple,
  "Half Kelly Long-Only" = kelly_half_long,
  "Full Kelly Long-Only" = kelly_full_long
)

cat("\n=== Portfolio Strategies Defined ===\n")
cat(sprintf("Number of strategies: %d\n", length(portfolio_list)))
cat("Strategies:", paste(names(portfolio_list), collapse = ", "), "\n")

# Prepare Dataset for backtesting


# portfolioBacktest requires a specific data structure
dataset_list <- list(port_data)

# Define backtesting parameters
# Use rolling windows for out-of-sample testing
lookback_periods <- 252 * 3  # 3 years for training
rebalance_freq <- 21 # Rebalance monthly

cat("\n=== Backtest Parameters ===\n")
cat(sprintf("Lookback period: %d days (~3 years)\n", lookback_periods))
cat(sprintf("Rebalance frequency: %d days (~monthly)\n", rebalance_freq))

# RUN PORTFOLIO BACKTEST

cat("\n=== Running Portfolio Backtest ===\n")
cat("This may take a few moments...\n\n")

bt_results <- portfolioBacktest::portfolioBacktest(
  portfolio_list,
  dataset_list,
  lookback = lookback_periods,
  optimize_every = rebalance_freq,
  rebalance_every = rebalance_freq,
  benchmarks = c("buy_and_hold" = buy_and_hold)
)


cat("✓ Backtest completed successfully!\n")

# Results and Performance

# Extract performance metrics table
performance_table <- backtestTable(bt_results)

cat("\n=== Backtest Performance Summary ===\n")
# print(performance_table)

# Extract individual backtest results for each strategy
bt_summary <- backtestSummary(bt_results)

print(bt_summary)

bt_summary %>%
  kable("html", booktabs = TRUE) %>%
  kable_styling(full_width = FALSE) %>%
  save_kable(file.path(output_dir, "backtest_summary.html"))

write.csv(bt_summary, 
          file.path(output_dir, "dji_portfoliobacktest_performance.csv"),
          row.names = TRUE)
cat(sprintf("\n✓ Saved: %s\n", file.path(output_dir, "dji_portfoliobacktest_performance.csv")))

# Graphs & Charts (Visualization)

# Chart 1: Cumulative Returns (native portfolioBacktest)
png(file.path(output_dir, "dji_cumulative_return.png"), 
    width = 10, height = 6, units = "in", res = 300)
print(backtestChartCumReturn(bt_results))
dev.off()
cat(sprintf("✓ Saved: %s\n", file.path(output_dir, "dji_cumulative_return.png")))

# Chart 2: Drawdown (native portfolioBacktest)
png(file.path(output_dir, "dji_drawdown.png"), 
    width = 10, height = 6, units = "in", res = 300)
print(backtestChartDrawdown(bt_results))
dev.off()
cat(sprintf("✓ Saved: %s\n", file.path(output_dir, "dji_drawdown.png")))

# Chart 3: Performance Bars
png(file.path(output_dir, "dji_performance_bars.png"), 
    width = 10, height = 6, units = "in", res = 300)
print(summaryBarPlot(bt_summary, 
                     measures = c("Sharpe ratio", "max drawdown", "annual return")))
dev.off()
cat(sprintf("✓ Saved: %s\n", file.path(output_dir, "dji_performance_bars.png")))

kelly_values <- seq(-1, 3, by = 0.05)   # from -100% to 300%

results <- sapply(kelly_values, function(f) {
  wealth <- cumprod(1 + f * returns_dj)
  tail(wealth, 1)
})

optimal_f <- kelly_values[which.max(results)]


cat(sprintf("Optimal Kelly Fraction for this Data is %.2f: ", optimal_f))
cat("\n✓✓✓ PORTFOLIO BACKTEST COMPLETE ✓✓✓\n")
cat(sprintf("Output directory: %s\n\n", output_dir))


