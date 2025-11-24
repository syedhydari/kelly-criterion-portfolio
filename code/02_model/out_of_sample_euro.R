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
library(lubridate)
library(scales)
library(patchwork)
library(tbl2xts)
library(quadprog)




# EuroStoxx Portfolio Optimization using Kelly Criterion


# Create output directory for figures
output_dir <- "output/figures/eurostoxx"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat(sprintf("Created output directory: %s\n", output_dir))
}


# Prepare EuroStoxx Data

# Source all functions
source("code/02_model/01_returns_calculation.R")

# File paths
PRICE_FILE <- "data/processed/prices_clean.csv"

# Load and Prepare Data

message("Step 1: Loading and preparing data...")

data <- prepare_kelly_data(PRICE_FILE)

# Filter date range
return_wide <- data$returns_wide
price <- data$prices_wide

# print(returns_analysis)


# returns_analysis |> 
#   select(DATE, ticker, return) |>
#   pivot_wider(names_from = ticker, values_from = return) -> return_wide

# print(port_data)

# port_data <- xts(x = port_data[, -1], order.by = port_data$date)


# print(port_data)

# Risk-free rate
rf_annual <- 0.01 # 0.01
rf_daily <- rf_annual / 252

cat("\n=== Data Downloaded ===\n")

# Kelly Opt Functions for Portfolio Backtesting - Analytical 
safe_kelly_fraction <- function(x, rf) {
  mu <- mean(x, na.rm = TRUE)
  sigma2 <- var(x, na.rm = TRUE)
  if (is.na(mu) || is.na(sigma2) || sigma2 == 0) return(0)
  return((mu - rf) / sigma2)
}

# kelly_strategy_factory <- function(multiplier) {
#   function(dataset, ...) {

#     ret <- dataset$returns

#     # Safety checks
#     if (is.null(ret) || ncol(ret) == 0) {
#       stop("Dataset passed to Kelly strategy has no returns columns.")
#     }

#     port_ret <- rowMeans(ret, na.rm = TRUE)

#     f <- safe_kelly_fraction(port_ret, rf_daily) * multiplier

#     # Cap leverage
#     f <- max(min(f, 3), -3)

#     # equal weights
#     w <- rep(1 / ncol(ret), ncol(ret)) * f

#     # Final safety
#     if (any(!is.finite(w))) {
#       w <- rep(1 / ncol(ret), ncol(ret))
#     }

#     names(w) <- colnames(ret)
#     return(w)
#   }
# }

kelly_strategy_factory <- function(multiplier, allow_short = TRUE) {

  function(dataset, ...) {
    ret <- dataset$returns

    # Base portfolio (equal weight, fully invested)
    w_base <- rep(1/ncol(ret), ncol(ret))

    # Portfolio returns from base weights
    port_ret <- rowMeans(ret, na.rm = TRUE)

    # Kelly fraction
    f <- safe_kelly_fraction(port_ret, rf_daily) * multiplier

    # Long-only clamp
    if (!allow_short) {
      f <- max(min(f, 1), 0)   # 0 ≤ f ≤ 1
    }
     if (allow_short) {
      f <- max(min(f, 3), -3)
    }

    # Position-sizing using Kelly multiplier!
    w <- w_base * f            

    # If long-only + fully invested constraint:
    # Scale remaining budget into risk-free asset
    if (!allow_short) {
      cash_weight <- 1 - sum(w)
      if (cash_weight < 0) cash_weight <- 0
    }   

    names(w) <- colnames(ret)
    return(w)
  }
}

# # Regularized inverse (handles singular covariance)
# safe_solve <- function(Sigma, lambda = 1e-4) {
#   n <- ncol(Sigma)
#   Sigma_reg <- Sigma + lambda * diag(n)
#   solve(Sigma_reg)
# }

# kelly_strategy_factory <- function(multiplier = 1) {

#   function(dataset, ...) {

#     ret <- dataset$returns

#     if (ncol(ret) < 1) stop("returns matrix malformed")

#     mu <- colMeans(ret, na.rm = TRUE)
#     Sigma <- cov(ret, use = "pairwise.complete.obs")

#     # Compute the global Kelly portfolio
#     excess_mu <- mu - rf_daily
#     invSigma  <- safe_solve(Sigma)

#     w_raw <- invSigma %*% excess_mu

#     # Apply Kelly multiplier
#     w <- as.numeric(w_raw * multiplier)

#     # Name weights
#     names(w) <- colnames(ret)

#     return(w)
#   }
# }




kelly_full   <- kelly_strategy_factory(multiplier = 1)
kelly_half   <- kelly_strategy_factory(multiplier = 0.5)
kelly_double <- kelly_strategy_factory(multiplier = 2)
kelly_triple <- kelly_strategy_factory(multiplier = 3)



# 5. Buy and Hold (100% in risky asset)
buy_and_hold <- function(dataset, ...) {
  prices <- dataset$adjusted
  w <- rep(1.0, ncol(prices))
  names(w) <- colnames(prices)
  
  return(w)
}


# BackTesting Portfolio List

portfolio_list <- list(
  # "Buy & Hold"   = buy_and_hold,
  "Half Kelly"   = kelly_half,
  "Full Kelly"   = kelly_full,
  "Double Kelly" = kelly_double,
  "Triple Kelly" = kelly_triple
)

cat("\n=== Portfolio Strategies Defined ===\n")
cat(sprintf("Number of strategies: %d\n", length(portfolio_list)))
cat("Strategies:", paste(names(portfolio_list), collapse = ", "), "\n")

# Prepare Dataset for backtesting

price <- xts(
  price[ , -1],           # all columns except DATE
  order.by = as.Date(price$DATE)
)

return_wide <- xts(
  return_wide[ , -1],           # all columns except DATE
  order.by = as.Date(return_wide$DATE)
)

dataset_list <- list(
  "EUROSTOXX" = list(
    prices = price,
    returns = return_wide
  )
)

print(dataset_list)

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
  rebalance_every = rebalance_freq
  #shortselling = FALSE
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

# Graphs & Charts (Visualization)


# Chart 1: Cumulative Returns (native portfolioBacktest)
png(file.path(output_dir, "euro_cumulative_return.png"), 
    width = 10, height = 6, units = "in", res = 300)
print(backtestChartCumReturn(bt_results))
dev.off()
cat(sprintf("✓ Saved: %s\n", file.path(output_dir, "euro_cumulative_return.png")))

# Chart 2: Drawdown (native portfolioBacktest)
png(file.path(output_dir, "euro_drawdown.png"), 
    width = 10, height = 6, units = "in", res = 300)
print(backtestChartDrawdown(bt_results))
dev.off()
cat(sprintf("✓ Saved: %s\n", file.path(output_dir, "euro_drawdown.png")))

# Chart 3: Performance Bars
png(file.path(output_dir, "euro_performance_bars.png"), 
    width = 10, height = 6, units = "in", res = 300)
print(summaryBarPlot(bt_summary, 
                     measures = c("Sharpe ratio", "max drawdown", "annual return")))
dev.off()
cat(sprintf("✓ Saved: %s\n", file.path(output_dir, "euro_performance_bars.png")))

# Chart 4: Stacked Bars
png(file.path(output_dir, "dji_fullkelly_bars.png"), 
    width = 10, height = 6, units = "in", res = 300)
print(backtestChartStackedBar(bt_results, portfolio = "Full Kelly", legend = TRUE))
dev.off()
cat(sprintf("✓ Saved: %s\n", file.path(output_dir, "dji_fullkelly_bars.png")))

png(file.path(output_dir, "dji_halfkelly_bars.png"), 
    width = 10, height = 6, units = "in", res = 300)
print(backtestChartStackedBar(bt_results, portfolio = "Half Kelly", legend = TRUE))
dev.off()
cat(sprintf("✓ Saved: %s\n", file.path(output_dir, "dji_halfkelly_bars.png")))

cat("✓✓✓ PORTFOLIO BACKTEST COMPLETE ✓✓✓\n")
cat(sprintf("Output directory: %s\n\n", output_dir))


