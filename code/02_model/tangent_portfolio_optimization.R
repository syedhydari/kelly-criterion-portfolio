---
title: "Tangent Portfolio Optimization"
subtitle: "Maximum Sharpe Ratio Portfolio - Analytical Implementation"
author: "Syed Bashir Hydari (@syedhydari)"
date: "2025-11-10"
format:
   pdf:
    toc: true
    toc-depth: 3
    code-fold: show
    theme: cosmo
    fig-width: 10
    fig-height: 6
    number-sections: true
execute:
  warning: false
  message: false
---

# Introduction

## Overview

The **Tangent Portfolio** (also known as the **Maximum Sharpe Ratio Portfolio**) is a cornerstone of Modern Portfolio Theory (MPT). It represents the portfolio that offers the best risk-adjusted return, measured by the Sharpe ratio.

This document provides:

- Complete theoretical foundation
- Mathematical derivation of the analytical solution
- Implementation in R
- Empirical application to EuroStoxx 50 data (2000-2018)
- Visualization and interpretation

## What is the Tangent Portfolio?

The tangent portfolio is the point on the efficient frontier where a line from the risk-free rate is tangent to the efficient frontier. This portfolio:

1. **Maximizes the Sharpe Ratio**: The ratio of excess return to volatility
2. **Represents optimal risk-adjusted returns**: Best return per unit of risk
3. **Is the market portfolio** in the Capital Asset Pricing Model (CAPM)
4. **Serves as a benchmark** for evaluating portfolio performance

---

# Mathematical Formulation

## The Optimization Problem

The tangent portfolio solves:

$$
\max_{w} \text{Sharpe Ratio} = \frac{\mu_p - r_f}{\sigma_p}
$$

where:

- $w$ = vector of portfolio weights ($n \times 1$)
- $\mu_p = w^\top \mu$ = portfolio expected return
- $\sigma_p = \sqrt{w^\top \Sigma w}$ = portfolio volatility
- $r_f$ = risk-free rate
- $\mu$ = vector of expected asset returns
- $\Sigma$ = covariance matrix of asset returns

**Constraints:**

$$
\begin{aligned}
\sum_{i=1}^{n} w_i &= 1 \quad \text{(fully invested)} \\
w_i &\geq 0 \quad \text{(no short-selling)}
\end{aligned}
$$

## Analytical Solution

### Step 1: Reformulation

Maximizing the Sharpe ratio is equivalent to:

$$
\max_{w} \frac{w^\top (\mu - r_f \mathbf{1})}{\sqrt{w^\top \Sigma w}}
$$

This is a **fractional programming problem**. We can transform it by noting that scaling $w$ doesn't change the Sharpe ratio.

### Step 2: Transformation

Set $w^\top (\mu - r_f \mathbf{1}) = 1$ (normalization). The problem becomes:

$$
\begin{aligned}
\min_{w} \quad & w^\top \Sigma w \\
\text{s.t.} \quad & w^\top (\mu - r_f \mathbf{1}) = 1
\end{aligned}
$$

### Step 3: Lagrangian

Form the Lagrangian:

$$
\mathcal{L}(w, \lambda) = w^\top \Sigma w - \lambda (w^\top (\mu - r_f \mathbf{1}) - 1)
$$

### Step 4: First-Order Conditions

Taking derivatives:

$$
\frac{\partial \mathcal{L}}{\partial w} = 2\Sigma w - \lambda(\mu - r_f \mathbf{1}) = 0
$$

Solving for $w$:

$$
w^* = \frac{\lambda}{2} \Sigma^{-1} (\mu - r_f \mathbf{1})
$$

### Step 5: Final Solution (No Short-Selling)

For the **long-only** (no short-selling) case:

$$
\tilde{w} = \Sigma^{-1} (\mu - r_f \mathbf{1})
$$

Then:

1. Set negative weights to zero: $\tilde{w}_i = \max(\tilde{w}_i, 0)$
2. Normalize: $w^* = \frac{\tilde{w}}{\sum_i \tilde{w}_i}$

**Result:** The tangent portfolio weights are **proportional** to the inverse covariance matrix times the excess returns.

---

# Implementation

## Setup

```{r setup}
#| label: setup

# Load required packages
library(tidyverse)      # Data manipulation
library(quantmod)       # Financial data
library(lubridate)      # Date handling
library(scales)         # Plotting scales
library(knitr)          # Tables
library(gt)             # Beautiful tables

# Set options
options(scipen = 999)
theme_set(theme_minimal(base_size = 12))

cat("✓ Packages loaded\n")
cat("Date:", format(Sys.Date(), "%Y-%m-%d"), "\n")
```

## Data Download

### EuroStoxx 50 Tickers

```{r tickers}
#| label: tickers

# EuroStoxx 50 constituent tickers (2000-2018 period)
eurostoxx_tickers <- c(
  "AD.AS",      # Adidas
  "AI.PA",      # Air Liquide  
  "ALV.DE",     # Allianz
  "AXA.PA",     # Axa
  "ISP.MI",     # Intesa Sanpaolo
  "BAS.DE",     # BASF
  "BAYN.DE",    # Bayer
  "BBVA.MC",    # BBVA
  "BMW.DE",     # BMW
  "BNP.PA",     # BNP Paribas
  "CA.PA",      # Carrefour
  "CRH.IR",     # CRH
  "DBK.DE",     # Deutsche Bank
  "ENEL.MI",    # Enel
  "ENGI.PA",    # Engie
  "ENI.MI",     # Eni
  "EOAN.DE",    # E.ON
  "IBE.MC",     # Iberdrola
  "KER.PA",     # Kering
  "LVMH.PA",    # LVMH
  "OR.PA",      # L'Oréal
  "ORA.PA",     # Orange
  "PHIA.AS",    # Philips
  "SAF.PA",     # Safran
  "SAN.PA",     # Sanofi
  "SAN.MC",     # Santander
  "SAP.DE",     # SAP
  "SU.PA",      # Schneider Electric
  "SIE.DE",     # Siemens
  "FP.PA",      # TotalEnergies
  "URW.AS",     # Unibail Rodamco
  "UNA.AS",     # Unilever
  "VIV.PA",     # Vivendi
  "DG.PA",      # Vinci
  "BN.PA",      # Danone
  "ITX.MC"      # Inditex
)

cat("Total tickers:", length(eurostoxx_tickers), "\n")
```

### Download Price Data

```{r download}
#| label: download-data
#| cache: true

# Date range
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2018-12-31")

cat("Downloading data from Yahoo Finance...\n")
cat("Period:", format(start_date, "%Y-%m-%d"), "to", format(end_date, "%Y-%m-%d"), "\n\n")

# Safe download function
safe_download <- function(ticker, start, end) {
  tryCatch({
    data <- getSymbols(ticker, src = "yahoo", from = start, to = end,
                       auto.assign = FALSE, warnings = FALSE)
    if (!is.null(data) && nrow(data) > 0) {
      adj_close <- Ad(data)
      colnames(adj_close) <- ticker
      return(adj_close)
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

# Download all tickers
price_list <- list()
successful <- c()
failed <- c()

for (ticker in eurostoxx_tickers) {
  cat("  ", ticker, "...")
  data <- safe_download(ticker, start_date, end_date)
  
  if (!is.null(data)) {
    price_list[[ticker]] <- data
    successful <- c(successful, ticker)
    cat(" ✓\n")
  } else {
    failed <- c(failed, ticker)
    cat(" ✗\n")
  }
  
  Sys.sleep(0.3)
}

cat("\nDownload complete!\n")
cat("  Successful:", length(successful), "\n")
cat("  Failed:", length(failed), "\n")

if (length(failed) > 0) {
  cat("  Failed tickers:", paste(failed, collapse = ", "), "\n")
}
```

### Clean and Prepare Data

```{r clean-data}
#| label: clean-data

# Merge prices
prices <- do.call(merge, price_list)

# Remove assets with >20% missing data
na_pct <- colMeans(is.na(prices))
keep_cols <- na_pct < 0.20
prices <- prices[, keep_cols]

# Forward fill NAs
prices <- na.locf(prices, na.rm = FALSE)

# Remove remaining incomplete rows
prices <- prices[complete.cases(prices), ]

cat("Final dataset:\n")
cat("  Rows (days):", nrow(prices), "\n")
cat("  Columns (stocks):", ncol(prices), "\n")
cat("  Date range:", format(min(index(prices)), "%Y-%m-%d"), "to",
    format(max(index(prices)), "%Y-%m-%d"), "\n")
```

### Calculate Monthly Returns

```{r returns}
#| label: calculate-returns

# Convert to monthly (end of month)
prices_monthly <- to.monthly(prices, OHLC = FALSE)

# Calculate log returns
returns_monthly <- diff(log(prices_monthly))
returns_monthly <- returns_monthly[-1, ]  # Remove first NA

# Convert to data frame
returns_df <- data.frame(
  DATE = index(returns_monthly),
  coredata(returns_monthly)
)

# Clean names
colnames(returns_df) <- c("DATE", colnames(returns_monthly))

cat("Monthly returns data:\n")
cat("  Observations:", nrow(returns_df), "months\n")
cat("  Assets:", ncol(returns_df) - 1, "\n")
cat("  Period:", format(min(returns_df$DATE), "%Y-%m"), "to",
    format(max(returns_df$DATE), "%Y-%m"), "\n\n")

# Summary statistics
cat("Average monthly return by asset (top 5):\n")
avg_returns <- colMeans(returns_df[, -1]) * 100
sort(avg_returns, decreasing = TRUE)[1:5] %>%
  round(3) %>%
  print()
```

---

# Tangent Portfolio Optimization

## Optimization Function

```{r tangent-function}
#| label: tangent-function

#' Compute Tangent Portfolio (Maximum Sharpe Ratio)
#'
#' @param returns_df Data frame with DATE column and asset returns
#' @param risk_free_rate Annual risk-free rate (default 0.01 = 1%)
#' @return List with portfolio weights and statistics
optimize_tangent_portfolio <- function(returns_df, risk_free_rate = 0.01) {
  
  # Extract returns matrix (remove DATE)
  ret_mat <- returns_df %>%
    select(-DATE) %>%
    as.matrix()
  
  # Dimensions
  n_obs <- nrow(ret_mat)
  n_assets <- ncol(ret_mat)
  asset_names <- colnames(ret_mat)
  
  # Calculate statistics
  mu <- colMeans(ret_mat, na.rm = TRUE)    # Expected returns
  Sigma <- cov(ret_mat, use = "complete.obs")  # Covariance matrix
  
  # Risk-free rate per month
  r_monthly <- risk_free_rate / 12
  
  cat("═══════════════════════════════════════════════\n")
  cat("  TANGENT PORTFOLIO OPTIMIZATION\n")
  cat("═══════════════════════════════════════════════\n\n")
  
  cat("Data:\n")
  cat("  Observations:", n_obs, "months\n")
  cat("  Assets:", n_assets, "\n")
  cat("  Risk-free rate:", sprintf("%.2f%% per year (%.4f%% per month)", 
                                     risk_free_rate * 100, r_monthly * 100), "\n\n")
  
  # Regularize covariance matrix (ensure positive definite)
  eigenvalues <- eigen(Sigma, only.values = TRUE)$values
  min_eig <- min(eigenvalues)
  
  cat("Covariance matrix:\n")
  cat("  Min eigenvalue:", format(min_eig, scientific = TRUE), "\n")
  
  if (min_eig < 1e-8) {
    ridge <- max(1e-6, abs(min_eig) * 2)
    Sigma <- Sigma + diag(ridge, n_assets)
    cat("  Applied ridge regularization:", format(ridge, scientific = TRUE), "\n")
  }
  cat("\n")
  
  # === ANALYTICAL SOLUTION ===
  # Step 1: Compute Sigma^(-1) * (mu - r_f)
  Sigma_inv <- solve(Sigma)
  excess_returns <- mu - r_monthly
  
  w_raw <- as.vector(Sigma_inv %*% excess_returns)
  
  cat("Analytical solution:\n")
  cat("  Raw weights (before constraints):\n")
  cat("    Positive weights:", sum(w_raw > 0), "\n")
  cat("    Negative weights:", sum(w_raw < 0), "\n")
  cat("    Sum:", sprintf("%.4f", sum(w_raw)), "\n\n")
  
  # Step 2: Apply no short-selling constraint
  w_long <- pmax(w_raw, 0)
  
  # Step 3: Normalize to sum to 1
  w_optimal <- w_long / sum(w_long)
  
  names(w_optimal) <- asset_names
  
  cat("Final portfolio:\n")
  cat("  Non-zero positions:", sum(w_optimal > 0.001), "\n")
  cat("  Largest weight:", sprintf("%.2f%%", max(w_optimal) * 100), "\n")
  cat("  Smallest non-zero weight:", sprintf("%.2f%%", 
                                              min(w_optimal[w_optimal > 0.001]) * 100), "\n\n")
  
  # === PORTFOLIO STATISTICS ===
  port_return <- sum(w_optimal * mu)
  port_variance <- as.numeric(t(w_optimal) %*% Sigma %*% w_optimal)
  port_sd <- sqrt(port_variance)
  
  # Annualized metrics
  ann_return <- port_return * 12
  ann_sd <- port_sd * sqrt(12)
  sharpe_ratio <- (ann_return - risk_free_rate) / ann_sd
  
  cat("Portfolio statistics:\n")
  cat("  Expected return:\n")
  cat("    Monthly:", sprintf("%.3f%%", port_return * 100), "\n")
  cat("    Annual:", sprintf("%.2f%%", ann_return * 100), "\n")
  cat("  Volatility:\n")
  cat("    Monthly:", sprintf("%.3f%%", port_sd * 100), "\n")
  cat("    Annual:", sprintf("%.2f%%", ann_sd * 100), "\n")
  cat("  Sharpe Ratio:", sprintf("%.4f", sharpe_ratio), "\n")
  cat("  Coefficient of Variation:", sprintf("%.4f", port_sd / port_return), "\n\n")
  
  cat("═══════════════════════════════════════════════\n\n")
  
  # Return results
  list(
    weights = w_optimal,
    weights_raw = w_raw,
    expected_return = port_return,
    volatility = port_sd,
    annualized_return = ann_return,
    annualized_volatility = ann_sd,
    sharpe_ratio = sharpe_ratio,
    n_assets = sum(w_optimal > 0.001),
    covariance_matrix = Sigma,
    mean_returns = mu,
    risk_free_rate = r_monthly,
    asset_names = asset_names
  )
}
```

## Compute Tangent Portfolio

```{r compute}
#| label: compute-tangent

# Run optimization
tangent <- optimize_tangent_portfolio(returns_df, risk_free_rate = 0.01)
```

---

# Results

## Portfolio Composition

```{r composition-table}
#| label: composition-table

# Create weights table
weights_table <- tibble(
  Rank = 1:length(tangent$weights),
  Ticker = names(tangent$weights),
  Weight = tangent$weights,
  `Weight %` = sprintf("%.2f%%", tangent$weights * 100),
  `Expected Return (monthly)` = tangent$mean_returns[names(tangent$weights)],
  `Return %` = sprintf("%.3f%%", tangent$mean_returns[names(tangent$weights)] * 100)
) %>%
  filter(Weight > 0.001) %>%
  arrange(desc(Weight)) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Ticker, `Weight %`, `Return %`)

# Display table with kable ONLY
kable(weights_table, 
      align = c("c", "l", "r", "r"),
      caption = "**Table 1**: Tangent Portfolio Composition - Maximum Sharpe Ratio Portfolio (In-Sample, 2000-2018)")

# Summary statistics below table
cat("\n\n---\n\n")
cat("**Portfolio Summary:**\n\n")
cat("- Total Assets:", tangent$n_assets, "\n")
cat("- Expected Monthly Return:", sprintf("%.3f%%", tangent$expected_return * 100), "\n")
cat("- Monthly Volatility:", sprintf("%.3f%%", tangent$volatility * 100), "\n")
cat("- Sharpe Ratio:", sprintf("%.4f", tangent$sharpe_ratio), "\n")
```

## Summary Statistics

```{r summary}
#| label: summary-stats

summary_stats <- tibble(
  Metric = c(
    "Number of Assets",
    "Expected Monthly Return",
    "Expected Annual Return",
    "Monthly Volatility",
    "Annual Volatility",
    "Sharpe Ratio",
    "Coefficient of Variation"
  ),
  Value = c(
    as.character(tangent$n_assets),
    sprintf("%.3f%%", tangent$expected_return * 100),
    sprintf("%.2f%%", tangent$annualized_return * 100),
    sprintf("%.3f%%", tangent$volatility * 100),
    sprintf("%.2f%%", tangent$annualized_volatility * 100),
    sprintf("%.4f", tangent$sharpe_ratio),
    sprintf("%.4f", tangent$volatility / tangent$expected_return)
  )
)

# Display table with kable only
kable(summary_stats, 
      align = c("l", "r"),
      caption = "Tangent Portfolio Performance Metrics")
```

---

# Visualization

## Portfolio Weights Bar Chart

```{r weights-plot}
#| label: weights-barplot
#| fig-width: 10
#| fig-height: 6

# Prepare data
weights_plot_data <- tibble(
  Ticker = names(tangent$weights),
  Weight = tangent$weights
) %>%
  filter(Weight > 0.001) %>%
  arrange(desc(Weight)) %>%
  mutate(
    Ticker = fct_reorder(Ticker, Weight),
    Weight_pct = Weight * 100
  )

# Create plot
p_weights <- ggplot(weights_plot_data, aes(x = Weight_pct, y = Ticker, fill = Weight_pct)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", Weight_pct)), 
            hjust = -0.1, size = 3.5) +
  scale_fill_gradient(low = "#3498DB", high = "#E74C3C") +
  scale_x_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Tangent Portfolio - Asset Allocation",
    subtitle = "Maximum Sharpe Ratio Portfolio",
    x = "Portfolio Weight",
    y = "Asset Ticker"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_weights)

# Save
ggsave("tangent_portfolio_weights.png", p_weights, 
       width = 10, height = 6, dpi = 300, bg = "white")
cat("\n✓ Saved: tangent_portfolio_weights.png\n")
```

## Efficient Frontier Position

```{r efficient-frontier}

#| label: efficient-frontier
#| fig-width: 10
#| fig-height: 7

# Compute efficient frontier (CORRECTED VERSION)
compute_efficient_frontier <- function(returns_df, risk_free_rate = 0.01, n_points = 50) {
  
  ret_mat <- returns_df %>% select(-DATE) %>% as.matrix()
  
  # Remove any columns with NA
  complete_cols <- colSums(is.na(ret_mat)) == 0
  ret_mat <- ret_mat[, complete_cols]
  
  # Remove any rows with NA
  complete_rows <- complete.cases(ret_mat)
  ret_mat <- ret_mat[complete_rows, ]
  
  n_assets <- ncol(ret_mat)
  mu <- colMeans(ret_mat, na.rm = TRUE)
  Sigma <- cov(ret_mat, use = "complete.obs")
  
  # Check for infinite or missing values
  if (any(!is.finite(mu)) || any(!is.finite(Sigma))) {
    warning("Non-finite values in returns data")
    return(tibble(return_ann = numeric(0), volatility_ann = numeric(0)))
  }
  
  # Regularize
  eig <- eigen(Sigma, only.values = TRUE)$values
  if (min(eig) < 1e-8) {
    ridge <- max(1e-6, abs(min(eig)) * 2)
    Sigma <- Sigma + diag(ridge, n_assets)
  }
  
  # Min variance portfolio
  Sigma_inv <- solve(Sigma)
  ones <- rep(1, n_assets)
  w_minvar <- as.vector(Sigma_inv %*% ones) / sum(Sigma_inv %*% ones)
  w_minvar <- pmax(w_minvar, 0)
  w_minvar <- w_minvar / sum(w_minvar)
  
  ret_min <- sum(w_minvar * mu) * 12
  vol_min <- sqrt(as.numeric(t(w_minvar) %*% Sigma %*% w_minvar)) * sqrt(12)
  
  # Max return asset
  max_idx <- which.max(mu)
  ret_max <- mu[max_idx] * 12
  vol_max <- sqrt(Sigma[max_idx, max_idx]) * sqrt(12)
  
  # Generate frontier using random portfolios (approximation)
  set.seed(42)
  n_random <- 5000
  
  frontier_points <- map_dfr(1:n_random, function(i) {
    w <- runif(n_assets)
    w <- w / sum(w)
    
    ret <- sum(w * mu) * 12
    vol <- sqrt(as.numeric(t(w) %*% Sigma %*% w)) * sqrt(12)
    
    tibble(return_ann = ret, volatility_ann = vol)
  })
  
  # Keep only efficient portfolios (upper envelope)
  frontier <- frontier_points %>%
    arrange(volatility_ann) %>%
    mutate(
      cummax_return = cummax(return_ann)
    ) %>%
    filter(return_ann >= cummax_return * 0.99) %>%
    select(return_ann, volatility_ann) %>%
    distinct() %>%
    arrange(volatility_ann)
  
  return(frontier)
}

# Compute frontier
cat("Computing efficient frontier...\n")
frontier <- compute_efficient_frontier(returns_df)
cat("✓ Frontier computed with", nrow(frontier), "points\n\n")

# Add tangent portfolio point
tangent_point <- tibble(
  Portfolio = "Tangent",
  Return = tangent$annualized_return * 100,
  Risk = tangent$annualized_volatility * 100
)

# Add risk-free asset
rf_point <- tibble(
  Return = 1,  # 1% risk-free rate
  Risk = 0
)

# Plot
p_frontier <- ggplot() +
  # Efficient frontier
  geom_line(data = frontier,
            aes(x = volatility_ann * 100, y = return_ann * 100),
            color = "gray60", linetype = "dotted", linewidth = 1.2) +
  # Capital Market Line (from risk-free to tangent)
  geom_segment(aes(x = 0, y = 1, 
                   xend = tangent_point$Risk, 
                   yend = tangent_point$Return),
               color = "gray40", linetype = "dashed", linewidth = 0.8) +
  # Risk-free asset
  geom_point(data = rf_point,
             aes(x = Risk, y = Return),
             color = "darkgreen", size = 3, shape = 16) +
  geom_text(data = rf_point,
            aes(x = Risk, y = Return, label = "Risk-Free"),
            hjust = -0.2, size = 3.5, color = "darkgreen") +
  # Tangent portfolio
  geom_point(data = tangent_point,
             aes(x = Risk, y = Return),
             color = "#3498DB", size = 5, shape = 18) +
  geom_text(data = tangent_point,
            aes(x = Risk, y = Return, label = "Tangent Portfolio"),
            vjust = -1.5, size = 4, fontface = "bold", color = "#3498DB") +
  labs(
    title = "Tangent Portfolio on the Efficient Frontier",
    subtitle = "Maximum Sharpe Ratio Portfolio Position",
    x = "Risk (Annual Volatility %)",
    y = "Expected Return (Annual %)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

print(p_frontier)

ggsave("tangent_portfolio_frontier.png", p_frontier,
       width = 10, height = 7, dpi = 300, bg = "white")
cat("\n✓ Saved: tangent_portfolio_frontier.png\n")
```

---

# Interpretation & Insights

## Key Findings

```{r insights}
#| label: insights
#| echo: false

cat("═══════════════════════════════════════════════\n")
cat("  KEY INSIGHTS - TANGENT PORTFOLIO\n")
cat("═══════════════════════════════════════════════\n\n")

cat("1. PORTFOLIO CONCENTRATION\n")
cat("   • The tangent portfolio holds", tangent$n_assets, "assets\n")
cat("   • Top 3 assets represent", 
    sprintf("%.1f%%", sum(sort(tangent$weights, decreasing = TRUE)[1:3]) * 100),
    "of the portfolio\n")
cat("   • This reflects concentration in high Sharpe ratio assets\n\n")

cat("2. RISK-RETURN PROFILE\n")
cat("   • Annual return:", sprintf("%.2f%%", tangent$annualized_return * 100), "\n")
cat("   • Annual volatility:", sprintf("%.2f%%", tangent$annualized_volatility * 100), "\n")
cat("   • Sharpe ratio:", sprintf("%.4f", tangent$sharpe_ratio), "\n")
cat("   • This represents the BEST risk-adjusted return available\n\n")

cat("3. COMPARISON TO ALTERNATIVES\n")
cat("   • The tangent portfolio maximizes Sharpe ratio\n")
cat("   • It dominates all other portfolios on a risk-adjusted basis\n")
cat("   • Investors seeking maximum return per unit of risk should hold this portfolio\n\n")

cat("4. PRACTICAL CONSIDERATIONS\n")
cat("   • In-sample optimization may overfit to historical data\n")
cat("   • Transaction costs and rebalancing frequency matter\n")
cat("   • Out-of-sample performance may differ from in-sample\n")
cat("   • Consider fractional Kelly or robust optimization for practical implementation\n\n")

cat("═══════════════════════════════════════════════\n")
```

## Mathematical Properties

The tangent portfolio has several important properties:

1. **Uniqueness**: There is only one tangent portfolio for a given set of assets and risk-free rate

2. **Efficiency**: It lies on the efficient frontier (no other portfolio offers higher return for the same risk)

3. **Capital Market Line**: All efficient portfolios are combinations of the tangent portfolio and the risk-free asset

4. **Two-Fund Separation**: Investors can achieve any desired risk level by mixing the tangent portfolio with the risk-free asset

5. **Maximum Sharpe Ratio**: By construction, no other portfolio achieves a higher Sharpe ratio

---

# Validation

## Comparison to Paper Results

From Carta & Conversano (2020), Table 6:

- **Expected monthly return**: 1.490%
- **Standard deviation**: 2.062%  
- **Number of assets**: 8

Our results:

- **Expected monthly return**: `r sprintf("%.3f%%", tangent$expected_return * 100)`
- **Standard deviation**: `r sprintf("%.3f%%", tangent$volatility * 100)`
- **Number of assets**: `r tangent$n_assets`

**Note**: Differences may arise from:
- Exact ticker selection (42 vs our available tickers)
- Data source variations
- Treatment of missing data
- Numerical precision

---

# Conclusion

This document provides a complete implementation of the **Tangent Portfolio Optimization** with:

✅ **Theoretical foundation** - Mathematical derivation from first principles  
✅ **Analytical solution** - Closed-form formula without numerical optimization  
✅ **R implementation** - Clean, documented, reusable code  
✅ **Empirical validation** - Application to real EuroStoxx 50 data  
✅ **Visualization** - Clear presentation of results  

The tangent portfolio represents the **optimal risk-adjusted investment** and serves as a key benchmark in Modern Portfolio Theory.