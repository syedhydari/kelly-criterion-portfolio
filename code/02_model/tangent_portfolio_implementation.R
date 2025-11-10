---
title: "Tangent Portfolio Optimization: Methods"
subtitle: "Carta & Conversano (2020) — Section 2.2.3"
author: "Syed Hydari"
format: 
  pdf:
    toc: false
    toc-depth: 2
    number-sections: true
    geometry: margin=1in
    fig-width: 7
    fig-height: 4.5
    fig-pos: 'h'
    code-line-numbers: true
execute:
  warning: false
  message: false
  echo: true
---

# Introduction

This document implements the **Tangent Portfolio (Maximum Sharpe Ratio)** optimization method as described in Carta & Conversano (2020), Section 2.2.3: "Implementing Kelly Criterion Portfolios."

The Tangent Portfolio represents the **Mean-Variance (Markowitz) control benchmark** used to compare against Kelly Criterion strategies. This implementation focuses exclusively on the **methodological building block** — the optimization algorithm itself — before experimental application.

This document implements the **Tangent Portfolio optimization algorithm** 
as described in Carta & Conversano (2020), Section 2.2.3. 

**This is a methodological implementation**, demonstrating:
- The optimization formulation
- Numerical solution approach
- Constraint handling
- Algorithm validation

This is **NOT** a replication of the paper's experimental results (Section 3), 
which would require:
- Rolling-window backtesting
- Multiple rebalancing periods  
- Ex-post performance tracking

The paper's Table 6 shows **in-sample characteristics** using the authors' 
Bloomberg data. This implementation uses Yahoo Finance data, which is 
adequate for demonstrating the **methodology** but results in different 
numerical values due to data source differences.

## Theoretical Framework

The Tangent Portfolio solves:

$$
\max_{w} \frac{(w' \mu - r_f)}{\sqrt{w' \Sigma w}}
$$

Subject to:
$$
\begin{aligned}
\sum_{i=1}^{n} w_i &= 1 \quad \text{(fully invested)} \\
w_i &\geq 0 \quad \forall i \quad \text{(long-only, no shorts)}
\end{aligned}
$$

Where:
- $w$ = portfolio weight vector
- $\mu$ = vector of expected returns
- $r_f$ = risk-free rate
- $\Sigma$ = covariance matrix of returns

This is equivalent to the paper's **Equation (13)** when expressed in terms of portfolio fractions $f_i$.

---

# Setup

```{r setup}

# ============================================================================
# Core Libraries
# ============================================================================
library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(quadprog)          # QP solver for constrained optimization
library(ggplot2)
library(scales)
library(tibble)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(gridExtra)  # For grid.arrange() in sensitivity plots

# ============================================================================
# Parameters (Aligned with Carta & Conversano 2020)
# ============================================================================
rf_annual    <- 0.01      # 1% annual risk-free rate (paper specification)
freq         <- 12        # Monthly frequency (12 periods/year)
ridge_lambda <- 1e-5      # Ridge regularization for Σ stabilization

set.seed(42)  # Reproducibility
```

---

# Data Loading

We use the **exact 8 stocks** from Carta & Conversano (2020) Table 6, which compose their Tangent Portfolio example from the EuroStoxx50 index.

```{r data-load}

# ============================================================================
# European Market Stocks
# ============================================================================
# Replicating Carta & Conversano (2020) using the 8 individual European stocks
# from their in-sample Tangent Portfolio analysis (Table 6, page 10)

tickers <- c(
  "DG.PA",      # Vinci (France) - Construction/concessions
  "ADS.DE",     # Adidas (Germany) - Sportswear
  "IBE.MC",     # Iberdrola (Spain) - Utilities
  "AI.PA",      # Air Liquide (France) - Industrial gases
  "UL",         # Unilever (US ADR) - Consumer goods
  "SAN.PA",     # Sanofi (France) - Pharmaceuticals
  "MC.PA",      # LVMH (France) - Luxury goods
  "BN.PA"       # Danone (France) - Food & beverages
)

stock_names <- c("Vinci", "Adidas", "Iberdrola", "AirLiquide", 
                 "Unilever", "Sanofi", "LVMH", "Danone")

# Match paper's exact time period
start_date <- "2000-01-01"
end_date   <- "2018-12-31"

cat("================================================================================\n")
cat("Downloading Paper's Original Stocks (Carta & Conversano 2020, Table 6)\n")
cat("================================================================================\n")
cat("Source: EuroStoxx50 constituents\n")
cat("Tickers:", paste(tickers, collapse = ", "), "\n")
cat("Period:", start_date, "to", end_date, "\n")
cat("Frequency: Monthly (as specified in paper)\n\n")

# ============================================================================
# Download Price Data with Error Handling
# ============================================================================
prices_list <- list()
failed_tickers <- c()

for (i in seq_along(tickers)) {
  ticker <- tickers[i]
  name <- stock_names[i]
  
  cat(sprintf("Downloading %s (%s)... ", name, ticker))
  
  prices_list[[name]] <- tryCatch({
    # Download daily data first
    data_daily <- getSymbols(ticker, src = "yahoo", 
                             from = start_date, 
                             to = end_date, 
                             auto.assign = FALSE, 
                             warnings = FALSE)
    
    # IMMEDIATELY extract month-end prices (last trading day of each month)
    month_ends <- endpoints(data_daily, on = "months")
    month_ends <- month_ends[month_ends > 0]  # Remove leading 0
    
    data_monthly <- data_daily[month_ends, ]
    
    # Extract adjusted close from monthly data
    adj_close <- Ad(data_monthly)
    
    cat("✓ Success (", nrow(adj_close), " months)\n")
    adj_close
    
  }, error = function(e) {
    cat("✗ Failed:", conditionMessage(e), "\n")
    failed_tickers <<- c(failed_tickers, paste0(name, " (", ticker, ")"))
    NULL
  })
}

# ============================================================================
# Handle Download Results
# ============================================================================
# Remove failed downloads
prices_list <- prices_list[!sapply(prices_list, is.null)]

cat("\n================================================================================\n")
cat("Download Summary\n")
cat("================================================================================\n")
cat("Successfully downloaded:", length(prices_list), "out of", length(tickers), "stocks\n")

if (length(failed_tickers) > 0) {
  cat("\n⚠ WARNING: Failed to download", length(failed_tickers), "ticker(s):\n")
  for (failed in failed_tickers) {
    cat("  •", failed, "\n")
  }
  cat("\nProceeding with", length(prices_list), "successfully downloaded stock(s).\n")
}

if (length(prices_list) == 0) {
  stop("\n✗ CRITICAL ERROR: Failed to download any stock data.\n",
       "  Please check:\n",
       "  1. Internet connection\n",
       "  2. Yahoo Finance availability\n",
       "  3. Ticker symbols validity\n")
}

if (length(prices_list) < 4) {
  warning("\n⚠ Only ", length(prices_list), " stocks downloaded. ",
          "Portfolio optimization may be unreliable with fewer than 4 assets.\n")
}

cat("\nSuccessfully proceeding with assets:\n")
cat("  ", paste(names(prices_list), collapse = ", "), "\n\n")

# ============================================================================
# Merge Price Data
# ============================================================================
prices <- do.call(merge.xts, prices_list)

# Remove rows with any missing data
prices_complete <- prices[complete.cases(prices), ]

if (nrow(prices_complete) < nrow(prices)) {
  n_removed <- nrow(prices) - nrow(prices_complete)
  cat("Note: Removed", n_removed, "observation(s) with missing data\n\n")
}

prices <- prices_complete

if (nrow(prices) < 24) {
  stop("Insufficient data: Only ", nrow(prices), 
       " observations available. Need at least 24 months.")
}

# ============================================================================
# Compute Monthly Returns (Data Already Monthly from Download)
# ============================================================================
cat("Using month-end prices (extracted during download)...\n")

# Data is already monthly from the download step
prices_monthly <- prices

cat("Month-end observations:", nrow(prices_monthly), "\n")
cat("Expected for 2000-2018: ~228 months\n")

# Verify monthly spacing
if (nrow(prices_monthly) > 1) {
  time_between <- mean(diff(as.numeric(index(prices_monthly))))
  cat("Average days between observations:", round(time_between, 1), "\n")
  
  if (time_between >= 20 && time_between <= 35) {
    cat("✓ Confirmed monthly frequency\n\n")
  } else {
    cat("⚠ Unexpected spacing (expected ~30 days)\n\n")
  }
} else {
  cat("\n")
}

# Calculate returns between month-ends
R <- Return.calculate(prices_monthly, method = "discrete")
R <- na.omit(R)

cat("Monthly returns calculated:\n")
cat("  Observations:", nrow(R), "\n")
cat("  Assets:", ncol(R), "\n\n")

# ============================================================================
# Data Summary
# ============================================================================
cat("\n================================================================================\n")
cat("Final Dataset Summary\n")
cat("================================================================================\n")
cat("Number of assets:    ", ncol(R), "\n")
cat("Number of periods:   ", nrow(R), "\n")
cat("Start date:          ", format(start(R)), "\n")
cat("End date:            ", format(end(R)), "\n")
cat("Frequency:           ", "Monthly\n")
cat("Total observations:  ", nrow(R) * ncol(R), "\n\n")

# Verify data quality
if (any(is.na(R))) {
  warning("Dataset contains NA values. Removing affected observations.")
  R <- na.omit(R)
}

if (any(is.infinite(as.matrix(R)))) {
  warning("Dataset contains infinite values. Check data quality.")
}

# ============================================================================
# Summary Statistics (Compare with Paper's Table 6)
# ============================================================================
cat("================================================================================\n")
cat("Asset Return Statistics\n")
cat("================================================================================\n\n")

# Clean asset names - remove ".Adjusted" suffix
clean_names <- gsub("\\.Adjusted$", "", colnames(R))

summary_stats <- data.frame(
  Asset = clean_names,
  Mean_Monthly_Pct = colMeans(R) * 100,
  Mean_Annual_Pct = colMeans(R) * 12 * 100,
  Vol_Monthly_Pct = apply(R, 2, sd) * 100,
  Vol_Annual_Pct = apply(R, 2, sd) * sqrt(12) * 100,
  Sharpe_Annual = (colMeans(R) * 12 - rf_annual) / (apply(R, 2, sd) * sqrt(12)),
  Min_Pct = apply(R, 2, min) * 100,
  Max_Pct = apply(R, 2, max) * 100
)

kable(summary_stats, 
      digits = 3, 
      caption = "Asset Return Statistics (Paper's Table 6 Stocks)",
      col.names = c("Asset", "Mean (%)", "Mean Ann. (%)", 
                    "Vol (%)", "Vol Ann. (%)", "Sharpe",
                    "Min (%)", "Max (%)")) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, 
                font_size = 10,
                latex_options = c("hold_position", "scale_down"))

# ============================================================================
# Comparison with Paper's Table 6 (If all 8 stocks downloaded)
# ============================================================================
if (ncol(R) == 8) {
  cat("\n================================================================================\n")
  cat("Comparison with Carta & Conversano (2020) Table 6\n")
  cat("================================================================================\n")
  cat("Paper's Tangent Portfolio (In-Sample, Monthly):\n")
  cat("  • Expected Return:  1.490%\n")
  cat("  • Std Deviation:    2.062%\n")
  cat("  • Coeff. Variation: 1.383\n\n")
  cat("Your Data (Same 8 Stocks, Monthly):\n")
  cat("  • Mean Return:      ", sprintf("%.3f%%", mean(colMeans(R)) * 100), "\n")
  cat("  • Mean Volatility:  ", sprintf("%.3f%%", mean(apply(R, 2, sd)) * 100), "\n")
  cat("\nNote: Minor differences expected due to:\n")
  cat("  • Data source (Yahoo Finance vs. Bloomberg/institutional)\n")
  cat("  • Adjusted price calculation methods\n")
  cat("  • Dividend treatment\n\n")
  
  # Store Paper's Table 6 values for later comparison
  paper_table6 <- list(
    return_monthly = 0.01490,
    vol_monthly = 0.02062,
    coef_var = 1.383
  )
  
  cat("Stored Paper's Table 6 values for post-optimization comparison.\n")
  
} else {
  cat("\nNote: ", ncol(R), " stocks successfully downloaded.\n")
  cat("Proceeding with optimization using available assets.\n\n")
}
```

# Data Hygiene Note

```{r data-quality-note}
# ============================================================================
# Data Quality Note
# ============================================================================
cat("\n================================================================================\n")
cat("Data Quality & Preprocessing\n")
cat("================================================================================\n\n")

cat("Data Source: Yahoo Finance (via quantmod package)\n")
cat("Preprocessing applied:\n")
cat("  • Extracted adjusted close prices (accounts for splits/dividends)\n")
cat("  • Converted to monthly frequency\n")
cat("  • Calculated discrete returns\n")
cat("  • Removed observations with missing data (na.omit)\n\n")

cat("Note: Yahoo Finance data quality considerations:\n")
cat("  • Generally reliable for major European stocks\n")
cat("  • Adjusted prices account for corporate actions\n")
cat("  • Minor differences vs. institutional data (Bloomberg) expected\n")
cat("  • For production use, additional cleaning recommended:\n")
cat("    - Outlier detection & winsorization\n")
cat("    - Stale price detection\n")
cat("    - Cross-validation with alternative sources\n\n")

cat("For this methods demonstration, standard preprocessing is sufficient.\n")
cat("================================================================================\n\n")
```

---

# Helper Functions

```{r helper-functions}

# ============================================================================
# Risk-Free Rate Conversion
# ============================================================================
rf_per_period <- function(rf_annual, freq) {
  (1 + rf_annual)^(1/freq) - 1
}

# ============================================================================
# Ridge-Stabilized Covariance Matrix
# ============================================================================
#' Compute covariance matrix with ridge regularization
#'
#' Adds small constant to diagonal for numerical stability and 
#' positive-definiteness guarantee.
#'
#' @param X Matrix of returns (T x N)
#' @param lambda Ridge parameter (default 1e-5)
#' @return Regularized covariance matrix
cov_ridge <- function(X, lambda = 1e-5) {
  S <- cov(X)
  p <- ncol(S)
  
  # Add ridge to diagonal
  S_ridge <- S + diag(lambda, p)
  
  # Validate positive-definiteness
  eig_vals <- eigen(S_ridge, symmetric = TRUE, only.values = TRUE)$values
  
  if (min(eig_vals) <= 0) {
    warning(sprintf(
      "Covariance not PD after ridge (min eigenvalue: %.2e). Increasing lambda.",
      min(eig_vals)
    ))
    S_ridge <- S + diag(lambda * 100, p)
  }
  
  S_ridge
}

# ============================================================================
# Simplex Projection (Condat 2016)
# ============================================================================
#' Project vector onto unit simplex
#'
#' Projects v onto {w : sum(w) = 1, w >= 0} using Condat (2016) algorithm.
#' Used for post-processing QP solutions to ensure exact constraint satisfaction.
#'
#' @param v Numeric vector
#' @return Projected vector on simplex
proj_simplex <- function(v) {
  n <- length(v)
  u <- sort(v, decreasing = TRUE)
  sv <- cumsum(u)
  rho <- max(which(u > (sv - 1) / seq_len(n)))
  theta <- (sv[rho] - 1) / rho
  w <- pmax(v - theta, 0)
  w / sum(w)  # Normalize to ensure exact sum = 1
}

# ============================================================================
# Sharpe Ratio Calculator
# ============================================================================
#' Compute Sharpe ratio for given weights
#'
#' @param w Weight vector
#' @param mu Expected return vector
#' @param Sigma Covariance matrix
#' @param rf Risk-free rate (per-period)
#' @return Sharpe ratio
sharpe_of_w <- function(w, mu, Sigma, rf) {
  excess_return <- sum(w * (mu - rf))
  variance <- drop(t(w) %*% Sigma %*% w)
  
  if (variance <= 0) return(-Inf)
  
  volatility <- sqrt(variance)
  excess_return / volatility
}

# ============================================================================
# Portfolio Characteristics
# ============================================================================
#' Compute portfolio mean, volatility, and Sharpe ratio
#'
#' @param w Weight vector
#' @param mu Expected return vector
#' @param Sigma Covariance matrix
#' @param rf Risk-free rate (per-period)
#' @param freq Periods per year (for annualization)
#' @return List with portfolio characteristics
portfolio_stats <- function(w, mu, Sigma, rf, freq = 12) {
  ret_monthly <- sum(w * mu)
  ret_annual <- ret_monthly * freq
  
  var_monthly <- drop(t(w) %*% Sigma %*% w)
  vol_monthly <- sqrt(var_monthly)
  vol_annual <- vol_monthly * sqrt(freq)
  
  # FIX: Calculate Sharpe directly here instead of calling sharpe_of_w with wrong args
  excess_return <- ret_monthly - rf
  sharpe <- (excess_return * freq) / vol_annual
  
  list(
    return_monthly = ret_monthly,
    return_annual = ret_annual,
    volatility_monthly = vol_monthly,
    volatility_annual = vol_annual,
    sharpe_ratio = sharpe
  )
}
```

---

# Core Optimization Algorithm

```{r tangent-optimizer}

# ============================================================================
# Maximum Sharpe Ratio Portfolio (Long-Only, Fully Invested)
# ============================================================================
#' Tangent Portfolio Optimization
#'
#' Solves for portfolio weights that maximize Sharpe ratio subject to:
#'   - Long-only constraint: w >= 0
#'   - Fully-invested constraint: sum(w) = 1
#'
#' Uses quadratic programming with grid search over target excess returns.
#' This approach is numerically stable and handles the non-convex nature
#' of Sharpe ratio maximization.
#'
#' @param mu Vector of expected returns (per-period)
#' @param Sigma Covariance matrix of returns
#' @param rf Risk-free rate (per-period)
#' @param k_grid Grid of target excess returns (optional)
#' @param verbose Print optimization details (default FALSE)
#' @return Optimal weight vector (sums to 1, all >= 0)
max_sharpe_longonly <- function(mu, Sigma, rf, k_grid = NULL, verbose = FALSE) {
  
  p <- length(mu)
  one <- rep(1, p)
  mu_ex <- mu - rf
  
  # ============================================================================
  # Define Grid of Target Excess Returns
  # ============================================================================
  if (is.null(k_grid)) {
    # Identify positive excess returns
    positive_ex <- mu_ex[mu_ex > 0]
    
    if (length(positive_ex) == 0) {
      warning("No assets with positive excess returns. Returning equal weights.")
      return(rep(1/p, p))
    }
    
    # Grid from 5th percentile to 95th percentile of feasible excess returns
    min_ex <- quantile(positive_ex, 0.05)
    max_ex <- quantile(positive_ex, 0.95)
    
    k_grid <- seq(min_ex, max_ex, length.out = 50)
  }
  
  # ============================================================================
  # Quadratic Programming Formulation
  # ============================================================================
  # Minimize: (1/2) w' (2*Sigma) w - 0' w
  # Subject to: A' w >= b with first meq constraints as equality
  #
  # Equality constraints (meq = 2):
  #   1. sum(w) = 1
  #   2. (mu - rf)' w = k  (target excess return)
  # Inequality constraints:
  #   3. w >= 0
  
  Dmat <- 2 * as.matrix(Sigma)  # QP convention
  dvec <- rep(0, p)
  
  best <- list(SR = -Inf, w = rep(1/p, p), k = NA_real_)
  
  for (k in k_grid) {
    # Constraint matrix: each column is a constraint
    Amat <- cbind(one, mu_ex, diag(p))
    bvec <- c(1, k, rep(0, p))
    
    sol <- tryCatch(
      solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 2),
      error = function(e) NULL
    )
    
    if (is.null(sol)) next
    
    w <- sol$solution
    
    # Post-processing for numerical precision
    w <- pmax(w, 0)  # Ensure non-negativity
    if (sum(w) == 0) next
    
    # Project to simplex for exact constraint satisfaction
    w <- proj_simplex(w)
    
    # Evaluate Sharpe ratio
    SR <- sharpe_of_w(w, mu, Sigma, rf)
    
    if (is.finite(SR) && SR > best$SR) {
      best <- list(SR = SR, w = w, k = k)
    }
  }
  
  if (verbose) {
    cat("\n=== Optimization Results ===\n")
    cat("Optimal Sharpe Ratio:", round(best$SR, 4), "\n")
    cat("Target Excess Return:", round(best$k, 6), "\n")
    cat("Grid points evaluated:", length(k_grid), "\n")
  }
  
  # ============================================================================
  # Final Validation
  # ============================================================================
  w_final <- best$w
  
  # Check constraints
  sum_constraint <- abs(sum(w_final) - 1)
  neg_weights <- any(w_final < -1e-8)
  
  if (sum_constraint > 1e-6) {
    warning(sprintf("Sum constraint violated: |sum(w) - 1| = %.2e", sum_constraint))
    w_final <- w_final / sum(w_final)
  }
  
  if (neg_weights) {
    warning("Negative weights detected. Re-projecting to simplex.")
    w_final <- proj_simplex(w_final)
  }
  
  w_final
}
```

---

# Demonstration: Optimize Tangent Portfolio

```{r optimize-tangent}

# ============================================================================
# Compute Optimal Tangent Portfolio
# ============================================================================

rf_period <- rf_per_period(rf_annual, freq)

cat("\n=== Tangent Portfolio Optimization ===\n")
cat("Risk-free rate (annual):", percent(rf_annual, accuracy = 0.01), "\n")
cat("Risk-free rate (monthly):", sprintf("%.6f", rf_period), "\n")
cat("Ridge parameter (λ):", sprintf("%.2e", ridge_lambda), "\n\n")

# Prepare data
R_clean <- R[complete.cases(R), ]
X <- coredata(R_clean)

# Estimate parameters
mu <- colMeans(X)
Sigma <- cov_ridge(X, ridge_lambda)

cat("Sample size:", nrow(X), "observations\n")
cat("Number of assets:", ncol(X), "\n")
cat("Covariance matrix condition number:", 
    sprintf("%.2e", kappa(Sigma)), "\n")
cat("Smallest eigenvalue:", 
    sprintf("%.2e", min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values)), "\n\n")

# Optimize
w_tangent <- max_sharpe_longonly(mu, Sigma, rf = rf_period, verbose = TRUE)

# Compute portfolio characteristics
stats <- portfolio_stats(w_tangent, mu, Sigma, rf_period, freq)

# ============================================================================
# Display Results
# ============================================================================

cat("\n=== Optimal Portfolio Weights ===\n")
weights_df <- tibble(
  Asset = gsub("\\.Adjusted$", "", colnames(R_clean)),
  Weight = w_tangent,
  Weight_Pct = percent(w_tangent, accuracy = 0.01)
) |> arrange(desc(Weight))

kable(weights_df, 
      digits = 6,
      caption = "Tangent Portfolio — Optimal Weights") |>
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

cat("\n=== Portfolio Characteristics ===\n")
char_df <- tibble(
  Metric = c("Expected Return (Monthly)", 
             "Expected Return (Annual)",
             "Volatility (Monthly)",
             "Volatility (Annual)",
             "Sharpe Ratio (Annualized)"),
  Value = c(
    percent(stats$return_monthly, accuracy = 0.01),
    percent(stats$return_annual, accuracy = 0.01),
    percent(stats$volatility_monthly, accuracy = 0.01),
    percent(stats$volatility_annual, accuracy = 0.01),
    sprintf("%.4f", stats$sharpe_ratio)
  )
)

kable(char_df, 
      col.names = c("Metric", "Value"),
      caption = "Tangent Portfolio — Expected Performance") |>
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# ============================================================================
# Comparison with Paper's Table 6 Results
# ============================================================================
if (exists("paper_table6")) {
  cat("\n================================================================================\n")
  cat("Alignment Check: Your Results vs. Paper's Table 6\n")
  cat("================================================================================\n\n")
  
  # Calculate percentage differences
  diff_ret <- abs(stats$return_monthly - paper_table6$return_monthly) / paper_table6$return_monthly * 100
  diff_vol <- abs(stats$volatility_monthly - paper_table6$vol_monthly) / paper_table6$vol_monthly * 100
  
  your_coef_var <- stats$volatility_monthly / stats$return_monthly
  diff_cv <- abs(your_coef_var - paper_table6$coef_var) / paper_table6$coef_var * 100
  
  # Create comparison table
  comparison_table6 <- tibble(
    Metric = c("Expected Return (Monthly %)", 
               "Std Deviation (Monthly %)",
               "Coefficient of Variation"),
    Paper_Value = c(paper_table6$return_monthly * 100,
                    paper_table6$vol_monthly * 100,
                    paper_table6$coef_var),
    Your_Value = c(stats$return_monthly * 100,
                   stats$volatility_monthly * 100,
                   your_coef_var),
    Difference_Pct = c(diff_ret, diff_vol, diff_cv)
  )
  
  kable(comparison_table6,
        digits = 3,
        col.names = c("Metric", "Paper (Table 6)", "Your Result", "Diff (%)"),
        caption = "Comparison with Carta & Conversano (2020) Table 6") |>
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  full_width = FALSE)
  
  # Interpretation
  cat("\n=== Interpretation ===\n")
  
  if (diff_ret < 15 && diff_vol < 15) {
    cat("✓ EXCELLENT alignment (<15% difference)\n")
    cat("  Your implementation matches the paper's methodology closely.\n")
  } else if (diff_ret < 25 && diff_vol < 25) {
    cat("✓ GOOD alignment (<25% difference)\n")
    cat("  Minor differences expected due to data source variation.\n")
  } else {
    cat("⚠ LARGER differences (>25%)\n")
    cat("  Consider checking:\n")
    cat("  • Data quality (outliers, missing values)\n")
    cat("  • Ticker accuracy (especially Unilever: use UNA.AS not ULVR.L)\n")
    cat("  • Date range alignment\n")
  }
  
  cat("\nNote: Differences are expected and acceptable due to:\n")
  cat("  • Data source: Yahoo Finance vs. Bloomberg/institutional\n")
  cat("  • Price adjustments: Different corporate action handling\n")
  cat("  • Methodology is identical; inputs differ slightly\n\n")
}
```

---

# Validation & Diagnostics

```{r validation}

# ============================================================================
# Constraint Validation
# ============================================================================

cat("\n=== Constraint Validation ===\n")

sum_weights <- sum(w_tangent)
min_weight <- min(w_tangent)
max_weight <- max(w_tangent)

validation_df <- tibble(
  Constraint = c(
    "Sum of weights = 1",
    "All weights >= 0",
    "Minimum weight",
    "Maximum weight"
  ),
  Status = c(
    ifelse(abs(sum_weights - 1) < 1e-6, "✓ PASS", "✗ FAIL"),
    ifelse(min_weight >= -1e-8, "✓ PASS", "✗ FAIL"),
    sprintf("%.6f", min_weight),
    sprintf("%.6f", max_weight)
  )
)

kable(validation_df,
      col.names = c("Constraint", "Status"),
      caption = "Constraint Satisfaction Check") |>
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# ============================================================================
# Portfolio Concentration (Herfindahl-Hirschman Index)
# ============================================================================

hhi <- sum(w_tangent^2)
effective_n <- 1 / hhi

cat("\n=== Portfolio Concentration ===\n")
cat("Herfindahl-Hirschman Index (HHI):", round(hhi, 4), "\n")
cat("Effective Number of Assets:", round(effective_n, 2), "\n")
cat("(Lower HHI = more diversified; HHI=1/N for equal-weight)\n\n")

# ============================================================================
# Covariance Matrix Conditioning
# ============================================================================

eig_vals <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
cond_num <- max(eig_vals) / min(eig_vals)

cat("=== Covariance Matrix Diagnostics ===\n")
cat("Condition number:", sprintf("%.2e", cond_num), "\n")
cat("Largest eigenvalue:", sprintf("%.2e", max(eig_vals)), "\n")
cat("Smallest eigenvalue:", sprintf("%.2e", min(eig_vals)), "\n")
cat("Determinant:", sprintf("%.2e", det(Sigma)), "\n")

if (cond_num > 1e10) {
  cat("⚠ WARNING: Covariance matrix is ill-conditioned.\n")
  cat("  Consider increasing ridge parameter.\n")
} else {
  cat("✓ Covariance matrix is well-conditioned.\n")
}
```

---

# Visualization

```{r visualization}
#| fig-width: 10
#| fig-height: 6

# ============================================================================
# Weight Distribution
# ============================================================================

weights_plot_df <- tibble(
  Asset = factor(colnames(R_clean), levels = colnames(R_clean)[order(w_tangent, decreasing = TRUE)]),
  Weight = w_tangent
)

ggplot(weights_plot_df, aes(x = Asset, y = Weight, fill = Asset)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Tangent Portfolio: Optimal Weight Allocation",
    subtitle = sprintf("Sharpe Ratio = %.3f | HHI = %.3f | Effective N = %.1f assets",
                      stats$sharpe_ratio, hhi, effective_n),
    x = NULL,
    y = "Portfolio Weight"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================================
# Efficient Frontier (Illustration)
# ============================================================================

# Generate portfolios along efficient frontier
n_portfolios <- 100
target_returns <- seq(min(mu), max(mu), length.out = n_portfolios)

frontier_portfolios <- lapply(target_returns, function(target_ret) {
  # Minimum variance for given target return
  Dmat <- 2 * as.matrix(Sigma)
  dvec <- rep(0, ncol(R_clean))
  Amat <- cbind(rep(1, ncol(R_clean)), mu, diag(ncol(R_clean)))
  bvec <- c(1, target_ret, rep(0, ncol(R_clean)))
  
  sol <- tryCatch(
    solve.QP(Dmat, dvec, Amat, bvec, meq = 2),
    error = function(e) NULL
  )
  
  if (is.null(sol)) return(NULL)
  
  w <- sol$solution
  w <- pmax(w, 0)
  if (sum(w) == 0) return(NULL)
  w <- w / sum(w)
  
  list(
    ret = sum(w * mu) * freq,
    vol = sqrt(drop(t(w) %*% Sigma %*% w)) * sqrt(freq),
    sharpe = sharpe_of_w(w, mu, Sigma, rf_period)
  )
})

frontier_df <- bind_rows(frontier_portfolios) |>
  filter(!is.na(ret)) |>
  arrange(vol)

# Tangent portfolio point
tangent_point <- tibble(
  ret = stats$return_annual,
  vol = stats$volatility_annual,
  label = "Tangent"
)

ggplot() +
  geom_line(data = frontier_df, aes(x = vol, y = ret), 
            color = "steelblue", linewidth = 1) +
  geom_point(data = tangent_point, aes(x = vol, y = ret), 
             color = "red", size = 4) +
  geom_text(data = tangent_point, aes(x = vol, y = ret, label = label),
            vjust = -1, size = 5, fontface = "bold") +
  geom_hline(yintercept = rf_annual, linetype = "dashed", color = "darkgray") +
  annotate("text", x = max(frontier_df$vol) * 0.9, y = rf_annual, 
           label = "Risk-Free Rate", vjust = -0.5, color = "darkgray") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Efficient Frontier and Tangent Portfolio",
    subtitle = "Long-only, fully-invested portfolios",
    x = "Volatility (Annual)",
    y = "Expected Return (Annual)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))
```

---

# Sensitivity Analysis

```{r sensitivity}
#| fig-width: 10
#| fig-height: 5

# ============================================================================
# Sensitivity to Ridge Parameter
# ============================================================================

lambda_grid <- 10^seq(-8, -3, length.out = 20)

sensitivity_results <- lapply(lambda_grid, function(lambda) {
  Sigma_ridge <- cov_ridge(X, lambda)
  w <- max_sharpe_longonly(mu, Sigma_ridge, rf_period, verbose = FALSE)
  stats <- portfolio_stats(w, mu, Sigma_ridge, rf_period, freq)
  
  tibble(
    lambda = lambda,
    sharpe = stats$sharpe_ratio,
    ret = stats$return_annual,
    vol = stats$volatility_annual,
    hhi = sum(w^2),
    min_eig = min(eigen(Sigma_ridge, symmetric = TRUE, only.values = TRUE)$values)
  )
}) |> bind_rows()

# Plot
p1 <- ggplot(sensitivity_results, aes(x = lambda, y = sharpe)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_vline(xintercept = ridge_lambda, linetype = "dashed", color = "red") +
  scale_x_log10(labels = scientific_format()) +
  labs(title = "Sharpe Ratio vs. Ridge Parameter",
       x = "Ridge Parameter (lambda)", y = "Sharpe Ratio") +
  theme_minimal(base_size = 12)

p2 <- ggplot(sensitivity_results, aes(x = lambda, y = hhi)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_vline(xintercept = ridge_lambda, linetype = "dashed", color = "red") +
  scale_x_log10(labels = scientific_format()) +
  labs(title = "Concentration (HHI) vs. Ridge Parameter",
       x = "Ridge Parameter (lambda)", y = "HHI") +
  theme_minimal(base_size = 12)

gridExtra::grid.arrange(p1, p2, ncol = 2)

cat("\n=== Sensitivity Analysis Summary ===\n")
cat("Ridge parameter range tested:", 
    sprintf("%.2e to %.2e", min(lambda_grid), max(lambda_grid)), "\n")
cat("Sharpe ratio range:", 
    sprintf("%.4f to %.4f", min(sensitivity_results$sharpe), 
            max(sensitivity_results$sharpe)), "\n")
cat("Optimal λ (selected):", sprintf("%.2e", ridge_lambda), "\n")
```

---

# Comparison with Naive Strategies

```{r comparison}

# ============================================================================
# Benchmark Strategies
# ============================================================================

# Equal-weight
w_equal <- rep(1 / ncol(R_clean), ncol(R_clean))
stats_equal <- portfolio_stats(w_equal, mu, Sigma, rf_period, freq)

# Minimum variance
Dmat_minvar <- 2 * as.matrix(Sigma)
dvec_minvar <- rep(0, ncol(R_clean))
Amat_minvar <- cbind(rep(1, ncol(R_clean)), diag(ncol(R_clean)))
bvec_minvar <- c(1, rep(0, ncol(R_clean)))

sol_minvar <- solve.QP(Dmat_minvar, dvec_minvar, Amat_minvar, bvec_minvar, meq = 1)
w_minvar <- proj_simplex(pmax(sol_minvar$solution, 0))
stats_minvar <- portfolio_stats(w_minvar, mu, Sigma, rf_period, freq)

# Comparison table
comparison_df <- tibble(
  Portfolio = c("Tangent (Max Sharpe)", "Equal Weight", "Minimum Variance"),
  Return = c(stats$return_annual, stats_equal$return_annual, stats_minvar$return_annual),
  Volatility = c(stats$volatility_annual, stats_equal$volatility_annual, stats_minvar$volatility_annual),
  Sharpe = c(stats$sharpe_ratio, stats_equal$sharpe_ratio, stats_minvar$sharpe_ratio),
  HHI = c(sum(w_tangent^2), sum(w_equal^2), sum(w_minvar^2))
)

kable(comparison_df,
      digits = 4,
      col.names = c("Portfolio", "Return (%)", "Volatility (%)", "Sharpe", "HHI"),
      caption = "Strategy Comparison (Ex-Ante Characteristics)") |>
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) |>
  row_spec(1, bold = TRUE, color = "white", background = "#3498db")
```