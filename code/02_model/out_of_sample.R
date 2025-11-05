# Main script to run the Kelly Criterion replication study

# Load required libraries
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

# Source all functions
source("code/02_model/01_returns_calculation.R")
source("code/02_model/02_optimization_functions.R")
source("code/02_model/03_rolling_backtest.R")
source("code/02_model/04_visualization.R")

# ============================================================================
# PARAMETERS
# ============================================================================

# File paths
PRICE_FILE <- "data/processed/prices_clean.csv"

# Analysis parameters
RISK_FREE_RATE <- 0.01  # 1% annual
WINDOW_MONTHS <- 24     # 24-month rolling window
FREQUENCY <- "monthly"  # monthly, weekly, or daily

# Date range
START_DATE <- "2007-01-01"
END_DATE <- "2018-12-28"

# ============================================================================
# 1. LOAD AND PREPARE DATA
# ============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("KELLY CRITERION PORTFOLIO OPTIMIZATION")
message(paste(rep("=", 70), collapse = ""), "\n")

message("Step 1: Loading and preparing data...")

data <- prepare_kelly_data(PRICE_FILE)

# Filter date range
returns_analysis <- data$returns_long %>%
  filter(DATE >= ymd(START_DATE), DATE <= ymd(END_DATE))

# Quality check
returns_check <- returns_analysis %>%
  select(DATE, ticker, return) %>%
  pivot_wider(names_from = ticker, values_from = return)


quality_check <- check_data_quality(returns_check)

message(sprintf("\nAnalysis period: %s to %s", START_DATE, END_DATE))
message(sprintf("Number of tickers: %d", n_distinct(returns_analysis$ticker)))
message(sprintf("Total observations: %s", 
                format(nrow(returns_analysis), big.mark = ",")))

# ============================================================================
# 2. IN-SAMPLE PORTFOLIO OPTIMIZATION
# ============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("Step 2: In-Sample Portfolio Optimization")
message(paste(rep("=", 70), collapse = ""), "\n")

# Prepare in-sample data
returns_insample <- returns_analysis %>%
  select(DATE, ticker, return) %>%
  pivot_wider(names_from = ticker, values_from = return)

# Test different Kelly fractions - Currently Just used to test the function - will eventually be used for Monte Carlo Experiments
message("\n--- Comparing Kelly Fractions ---")

kelly_full <- optimize_kelly_long_only(returns_insample, RISK_FREE_RATE, 
                                        kelly_fraction = 1.0)
kelly_half <- optimize_kelly_long_only(returns_insample, RISK_FREE_RATE, 
                                        kelly_fraction = 0.5)
kelly_quarter <- optimize_kelly_long_only(returns_insample, RISK_FREE_RATE, 
                                           kelly_fraction = 0.25)

# Compare
comparison <- tibble(
  Fraction = c("Full (1.0)", "Half (0.5)", "Quarter (0.25)"),
  `Expected Return` = c(kelly_full$expected_return, 
                        kelly_half$expected_return,
                        kelly_quarter$expected_return) * 100,
  Volatility = c(kelly_full$volatility, 
                 kelly_half$volatility,
                 kelly_quarter$volatility) * 100,
  `Sharpe Ratio` = c(kelly_full$sharpe_ratio,
                     kelly_half$sharpe_ratio,
                     kelly_quarter$sharpe_ratio)
)

print(comparison)

# Optimize portfolios
message("\n--- Optimizing Kelly Portfolio (Long-Only) ---")
kelly_port <- optimize_kelly_long_only(returns_insample, RISK_FREE_RATE)

message("\n--- Optimizing Min-Variance Portfolio ---")
minvar_port <- optimize_min_variance(returns_insample)

message("\n--- Creating Equal-Weight Benchmark ---")
equal_port <- create_equal_weight_portfolio(returns_insample)

# Display compositions
message("\n" , paste(rep("-", 70), collapse = ""))
message("KELLY PORTFOLIO COMPOSITION")
message(paste(rep("-", 70), collapse = ""))
print(kelly_port$weights_df, n = 20)
message(sprintf("\nExpected Return: %.4f%% (annualized: %.2f%%)", 
                kelly_port$expected_return * 100,
                kelly_port$expected_return * 252 * 100))
message(sprintf("Volatility: %.4f%% (annualized: %.2f%%)", 
                kelly_port$volatility * 100,
                kelly_port$volatility * sqrt(252) * 100))
message(sprintf("Sharpe Ratio: %.4f", kelly_port$sharpe_ratio))
message(sprintf("Number of Assets: %d", kelly_port$n_assets))

message("\n", paste(rep("-", 70), collapse = ""))
message("MIN-VARIANCE PORTFOLIO COMPOSITION")
message(paste(rep("-", 70), collapse = ""))
print(minvar_port$weights_df, n = 20)
message(sprintf("\nExpected Return: %.4f%% (annualized: %.2f%%)", 
                minvar_port$expected_return * 100,
                minvar_port$expected_return * 252 * 100))
message(sprintf("Volatility: %.4f%% (annualized: %.2f%%)", 
                minvar_port$volatility * 100,
                minvar_port$volatility * sqrt(252) * 100))
message(sprintf("Number of Assets: %d", minvar_port$n_assets))

# Plot portfolio weights
p_kelly_weights <- plot_portfolio_weights(
  kelly_port$weights_df,
  title = "Kelly Portfolio (Long-Only) Composition"
)

p_minvar_weights <- plot_portfolio_weights(
  minvar_port$weights_df,
  title = "Min-Variance Portfolio Composition"
)

p_weights_combined <- p_kelly_weights / p_minvar_weights +
  plot_annotation(
    title = "In-Sample Portfolio Compositions",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave("output/figures/01_portfolio_weights.png", p_weights_combined,
       width = 10, height = 8, dpi = 300)

message("\nSaved: figures/01_portfolio_weights.png")

# Save compositions
write_csv(kelly_port$weights_df, "output/tables/kelly_insample_weights.csv")
write_csv(minvar_port$weights_df, "output/tables/minvar_insample_weights.csv")

# ============================================================================
# 3. ROLLING WINDOW BACKTEST
# ============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message(sprintf("Step 3: Rolling Window Backtest (%d months)", WINDOW_MONTHS))
message(paste(rep("=", 70), collapse = ""), "\n")

backtest_results <- rolling_backtest(
  returns_analysis,
  window_months = WINDOW_MONTHS,
  risk_free_rate = RISK_FREE_RATE
)

# Calculate metrics
metrics <- calculate_performance_metrics(
  backtest_results$returns,
  RISK_FREE_RATE
)

message("\n", paste(rep("-", 70), collapse = ""))
message("PERFORMANCE METRICS")
message(paste(rep("-", 70), collapse = ""))
print(metrics %>% 
        select(portfolio, cagr, annualized_vol, sharpe_ratio, final_wealth) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Calculate drawdowns
dd_results <- calculate_drawdowns(backtest_results$returns)

message("\n", paste(rep("-", 70), collapse = ""))
message("DRAWDOWN STATISTICS")
message(paste(rep("-", 70), collapse = ""))
print(dd_results$max_drawdowns %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Calculate information ratios
ir_results <- calculate_information_ratio(
  backtest_results$returns,
  benchmark = "EqualWeight"
)

message("\n", paste(rep("-", 70), collapse = ""))
message("INFORMATION RATIOS (vs Equal Weight)")
message(paste(rep("-", 70), collapse = ""))
print(ir_results %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Create performance table (Table 8 style)
perf_table <- create_performance_table(
  metrics,
  dd_results$max_drawdowns,
  ir_results
)

message("\n", paste(rep("-", 70), collapse = ""))
message("SUMMARY TABLE")
message(paste(rep("-", 70), collapse = ""))
print(perf_table, n = Inf)

write_csv(perf_table, "output/tables/performance_summary.csv")
write_csv(metrics, "output/tables/performance_metrics_detailed.csv")

# ============================================================================
# 4. VISUALIZATIONS
# ============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("Step 4: Creating Visualizations")
message(paste(rep("=", 70), collapse = ""), "\n")

# Cumulative returns (Figure 6 style)
p_cumulative <- plot_cumulative_returns(
  backtest_results$returns,
  title = sprintf("Out-of-Sample Cumulative Returns (%d-month Rolling Window)", 
                  WINDOW_MONTHS)
)

ggsave("output/figures/02_cumulative_returns.png", p_cumulative,
       width = 12, height = 6, dpi = 300)
message("Saved: output/figures/02_cumulative_returns.png")

# Drawdowns
p_drawdowns <- plot_drawdowns(
  dd_results$drawdowns,
  title = "Portfolio Drawdowns Over Time"
)

ggsave("output/figures/03_drawdowns.png", p_drawdowns,
       width = 12, height = 6, dpi = 300)
message("Saved: output/figures/03_drawdowns.png")

# Risk-return scatter
p_risk_return <- plot_risk_return_scatter(
  metrics,
  title = "Risk-Return Profile"
)

ggsave("output/figures/04_risk_return.png", p_risk_return,
       width = 8, height = 6, dpi = 300)
message("Saved: output/figures/04_risk_return.png")

# Rolling Sharpe
p_rolling_sharpe <- plot_rolling_sharpe(
  backtest_results$returns,
  window = 12,
  risk_free_rate = RISK_FREE_RATE
)

ggsave("output/figures/05_rolling_sharpe.png", p_rolling_sharpe,
       width = 12, height = 6, dpi = 300)
message("Saved: output/figures/05_rolling_sharpe.png")

# Combined dashboard
p_dashboard <- create_performance_dashboard(
  backtest_results$returns,
  dd_results$drawdowns,
  metrics
)

ggsave("output/figures/06_performance_dashboard.png", p_dashboard,
       width = 16, height = 12, dpi = 300)
message("Saved: output/figures/06_performance_dashboard.png")

# Combined cumulative + drawdown (paper style)
p_combined <- p_cumulative / p_drawdowns +
  plot_annotation(
    title = "Kelly vs Min-Variance: Out-of-Sample Performance",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave("output/figures/07_stacked_performance_plot.png", p_combined,
       width = 12, height = 10, dpi = 300)
message("Saved: output/figures/07_stacked_performance_plot.png")

# ============================================================================
# 5. SAVE ALL RESULTS
# ============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("Step 5: Saving Results")
message(paste(rep("=", 70), collapse = ""), "\n")

# Save portfolio returns
write_csv(backtest_results$returns, "output/tables/portfolio_returns.csv")
message("Saved: output/tables/portfolio_returns.csv")

# Save weights
write_csv(backtest_results$kelly_weights, "output/tables/kelly_rolling_weights.csv")
write_csv(backtest_results$minvar_weights, "output/tables/minvar_rolling_weights.csv")
message("Saved: output/tables/kelly_rolling_weights.csv")
message("Saved: output/tables/minvar_rolling_weights.csv")

# Save drawdowns
write_csv(dd_results$drawdowns, "output/tables/drawdowns.csv")
write_csv(dd_results$max_drawdowns, "output/tables/max_drawdowns.csv")
message("Saved: output/tables/drawdowns.csv")
message("Saved: output/tables/max_drawdowns.csv")

# ============================================================================
# 6. FINAL SUMMARY
# ============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("ANALYSIS COMPLETE!")
message(paste(rep("=", 70), collapse = ""))

message("\nKEY FINDINGS:")
message(sprintf("  Kelly CAGR: %.2f%%", metrics %>% filter(portfolio == "Kelly") %>% pull(cagr) * 100))
message(sprintf("  MinVar CAGR: %.2f%%", metrics %>% filter(portfolio == "MinVariance") %>% pull(cagr) * 100))
message(sprintf("  Equal Weight CAGR: %.2f%%", metrics %>% filter(portfolio == "EqualWeight") %>% pull(cagr) * 100))

message("\n  Kelly Final Wealth: $", sprintf("%.2f", metrics %>% filter(portfolio == "Kelly") %>% pull(final_wealth)))
message("  MinVar Final Wealth: $", sprintf("%.2f", metrics %>% filter(portfolio == "MinVariance") %>% pull(final_wealth)))
message("  Equal Weight Final Wealth: $", sprintf("%.2f", metrics %>% filter(portfolio == "EqualWeight") %>% pull(final_wealth)))

message("\nOUTPUT FILES:")
message("  Figures: output/figures/")
message("  Results: output/tables/")

message("\n", paste(rep("=", 70), collapse = ""), "\n")


