# Monte Carlo Simulation for Kelly Criterion
# Replicates Section 3.1 and Tables 1-4 from Carta & Conversano (2020)

library(tidyverse)
library(parallel)  # For faster computation

# ============================================================================
# PARAMETERS (From Paper Section 2.3)
# ============================================================================

# Annual parameters
MU_ANNUAL <- 0.12      # 12% annual return
SIGMA_ANNUAL <- 0.40   # 40% annual volatility
RF_ANNUAL <- 0.01      # 1% risk-free rate
TRADING_DAYS <- 252

# Convert to daily
MU_DAILY <- MU_ANNUAL / TRADING_DAYS
SIGMA_DAILY <- SIGMA_ANNUAL / sqrt(TRADING_DAYS)
RF_DAILY <- RF_ANNUAL / TRADING_DAYS

message(sprintf("Daily parameters:"))
message(sprintf("  μ = %.6f", MU_DAILY))
message(sprintf("  σ = %.6f", SIGMA_DAILY))
message(sprintf("  rf = %.6f", RF_DAILY))

# ============================================================================
# KELLY FRACTIONS (Equation 9)
# ============================================================================

# Full Kelly: f* = (μ - r) / σ²
F_FULL_KELLY <- (MU_DAILY - RF_DAILY) / (SIGMA_DAILY^2)

# Fractional Kelly strategies
F_HALF_KELLY <- 0.5 * F_FULL_KELLY
F_DOUBLE_KELLY <- 2.0 * F_FULL_KELLY
F_TRIPLE_KELLY <- 3.0 * F_FULL_KELLY

message(sprintf("\nKelly fractions:"))
message(sprintf("  Half Kelly:   f = %.4f", F_HALF_KELLY))
message(sprintf("  Full Kelly:   f = %.4f", F_FULL_KELLY))
message(sprintf("  Double Kelly: f = %.4f", F_DOUBLE_KELLY))
message(sprintf("  Triple Kelly: f = %.4f", F_TRIPLE_KELLY))

# ============================================================================
# GEOMETRIC BROWNIAN MOTION (Equation 14)
# ============================================================================

#' Generate GBM returns
#' 
#' @param n_periods Number of trading periods
#' @param mu Daily drift
#' @param sigma Daily volatility
#' @return Vector of returns
generate_gbm_returns <- function(n_periods, mu, sigma) {
  Z <- rnorm(n_periods)  # Standard normal draws
  returns <- (mu - sigma^2/2) + sigma * Z
  return(returns)
}

# ============================================================================
# WEALTH EVOLUTION (Equation 15)
# ============================================================================

#' Simulate wealth path with Kelly fraction
#' 
#' @param W0 Initial wealth
#' @param f Kelly fraction to invest in risky asset
#' @param returns Vector of returns
#' @param rf Risk-free rate per period
#' @return Vector of wealth values (length n+1)
simulate_wealth <- function(W0, f, returns, rf) {
  n <- length(returns)
  W <- numeric(n + 1)
  W[1] <- W0
  
  for(t in 1:n) {
    # Split wealth: (1-f) in risk-free, f in risky asset
    risk_free_amount <- W[t] * (1 - f)
    risky_amount <- W[t] * f
    
    # Update wealth
    W[t+1] <- risk_free_amount * exp(rf) + risky_amount * exp(returns[t])
  }
  
  return(W)
}

# ============================================================================
# MONTE CARLO SIMULATION
# ============================================================================

#' Run Monte Carlo simulation for given number of trades
#' 
#' @param n_trades Number of trading periods
#' @param n_trajectories Number of simulation paths
#' @param W0 Initial wealth
#' @return Data frame with simulation results
run_monte_carlo_simulation <- function(n_trades, n_trajectories, W0 = 1) {
  
  message(sprintf("\n=== Running simulation: %d trades, %d trajectories ===", 
                  n_trades, n_trajectories))
  
  # Define strategies
  strategies <- list(
    "Half Kelly" = F_HALF_KELLY,
    "Full Kelly" = F_FULL_KELLY,
    "Double Kelly" = F_DOUBLE_KELLY,
    "Triple Kelly" = F_TRIPLE_KELLY
  )
  
  results <- list()
  
  for(strategy_name in names(strategies)) {
    f <- strategies[[strategy_name]]
    
    message(sprintf("  Running %s (f=%.4f)...", strategy_name, f))
    
    # Storage for all trajectories
    final_wealth <- numeric(n_trajectories)
    time_to_double <- numeric(n_trajectories)
    time_to_tenfold <- numeric(n_trajectories)
    
    # Run simulations
    for(i in 1:n_trajectories) {
      # Generate returns
      returns <- generate_gbm_returns(n_trades, MU_DAILY, SIGMA_DAILY)
      
      # Simulate wealth path
      W <- simulate_wealth(W0, f, returns, RF_DAILY)
      
      # Store final wealth
      final_wealth[i] <- W[length(W)]
      
      # Find time to double (if achieved)
      double_idx <- which(W > 2 * W0)
      time_to_double[i] <- ifelse(length(double_idx) > 0, 
                                    min(double_idx) - 1, 
                                    NA)
      
      # Find time to 10x (if achieved)
      tenfold_idx <- which(W > 10 * W0)
      time_to_tenfold[i] <- ifelse(length(tenfold_idx) > 0, 
                                     min(tenfold_idx) - 1, 
                                     NA)
    }
    
    # Calculate statistics (matching Tables 1-4)
    results[[strategy_name]] <- list(
      strategy = strategy_name,
      n_trades = n_trades,
      n_trajectories = n_trajectories,
      f = f,
      
      # Final wealth statistics
      mean_wt = mean(final_wealth),
      median_wt = median(final_wealth),
      std_wt = sd(final_wealth),
      
      # Ruin probabilities
      prob_loss = mean(final_wealth < W0),
      prob_half_loss = mean(final_wealth < 0.5 * W0),
      prob_severe_loss = mean(final_wealth < 0.1 * W0),
      
      # Wealth goals
      prob_double = mean(final_wealth > 2 * W0),
      mean_time_double = mean(time_to_double, na.rm = TRUE),
      prob_tenfold = mean(final_wealth > 10 * W0),
      mean_time_tenfold = mean(time_to_tenfold, na.rm = TRUE)
    )
  }
  
  # Convert to data frame
  results_df <- bind_rows(results)
  
  return(results_df)
}

# ============================================================================
# CALCULATE MEAN LOG-WEALTH (For Figure 2)
# ============================================================================

#' Calculate mean log-wealth for different fractions
#' 
#' @param n_trades Number of trades
#' @param n_trajectories Number of simulations
#' @param fractions Vector of fractions to test
#' @return Data frame with fraction and mean log-wealth
calculate_mean_log_wealth <- function(n_trades, 
                                       n_trajectories = 10000,
                                       fractions = seq(0, 1.2, length.out = 100)) {
  
  message(sprintf("\nCalculating mean log-wealth curve (%d trades)...", n_trades))
  
  results <- tibble(
    fraction = fractions,
    mean_log_wealth = NA_real_
  )
  
  for(i in seq_along(fractions)) {
    f <- fractions[i]
    
    log_wealth_sum <- 0
    
    for(j in 1:n_trajectories) {
      returns <- generate_gbm_returns(n_trades, MU_DAILY, SIGMA_DAILY)
      W <- simulate_wealth(1, f, returns, RF_DAILY)
      log_wealth_sum <- log_wealth_sum + log(W[length(W)])
    }
    
    results$mean_log_wealth[i] <- log_wealth_sum / n_trajectories
    
    if(i %% 20 == 0) {
      message(sprintf("  Progress: %d/%d fractions", i, length(fractions)))
    }
  }
  
  return(results)
}

# ============================================================================
# MAIN EXECUTION: RUN ALL SIMULATIONS
# ============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("MONTE CARLO SIMULATION - KELLY CRITERION")
message("Replicating Carta & Conversano (2020) Tables 1-4")
message(paste(rep("=", 70), collapse = ""))

# Scenarios from the paper
scenarios <- list(
  list(n_trades = 100, n_trajectories = 10000, table_num = 1),
  list(n_trades = 1000, n_trajectories = 10000, table_num = 2),
  list(n_trades = 10000, n_trajectories = 10000, table_num = 3),
  list(n_trades = 40000, n_trajectories = 1000, table_num = 4)  # Reduced for computation
)

# Run all simulations
all_results <- list()

for(scenario in scenarios) {
  set.seed(123)  # For reproducibility
  
  results <- run_monte_carlo_simulation(
    n_trades = scenario$n_trades,
    n_trajectories = scenario$n_trajectories
  )
  
  all_results[[paste0("table_", scenario$table_num)]] <- results
}

# Combine all results
combined_results <- bind_rows(all_results, .id = "table")

# ============================================================================
# SAVE RESULTS
# ============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SAVING RESULTS")
message(paste(rep("=", 70), collapse = ""))

# Create output directory if needed
dir.create("output/tables/monte_carlo", recursive = TRUE, showWarnings = FALSE)

# Save combined results
write_csv(combined_results, "output/tables/monte_carlo/all_simulations.csv")
message("Saved: output/tables/monte_carlo/all_simulations.csv")

# Save individual tables (matching paper format)
for(i in 1:4) {
  table_data <- all_results[[paste0("table_", i)]]
  
  # Format for display (matching paper)
  table_formatted <- table_data %>%
    select(
      Strategy = strategy,
      `Mean(WT)` = mean_wt,
      `Median(WT)` = median_wt,
      `Std(WT)` = std_wt,
      `P(WT < 1)` = prob_loss,
      `P(WT < 0.5)` = prob_half_loss,
      `P(WT < 0.1)` = prob_severe_loss,
      `P(WT > 2)` = prob_double,
      `Mean(T:2)` = mean_time_double,
      `P(WT > 10)` = prob_tenfold,
      `Mean(T:10)` = mean_time_tenfold
    ) %>%
    mutate(across(where(is.numeric), ~round(., 4)))
  
  write_csv(table_formatted, 
            sprintf("output/tables/monte_carlo/table_%d_results.csv", i))
  message(sprintf("Saved: output/tables/monte_carlo/table_%d_results.csv", i))
}

# ============================================================================
# PRINT SUMMARY TABLES
# ============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SUMMARY TABLES (Matching Carta & Conversano 2020)")
message(paste(rep("=", 70), collapse = ""))

for(i in 1:4) {
  table_data <- all_results[[paste0("table_", i)]]
  n_trades <- unique(table_data$n_trades)
  
  message(sprintf("\n--- TABLE %d: Results after %d trades ---", i, n_trades))
  
  summary_table <- table_data %>%
    select(
      Strategy = strategy,
      `Mean(WT)` = mean_wt,
      `Median(WT)` = median_wt,
      `Std(WT)` = std_wt,
      `P(WT<1)` = prob_loss,
      `P(WT>2)` = prob_double,
      `P(WT>10)` = prob_tenfold
    ) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  print(summary_table)
}

message("\n", paste(rep("=", 70), collapse = ""))
message("KEY FINDINGS:")
message(paste(rep("=", 70), collapse = ""))

# Compare Full Kelly across scenarios
full_kelly_results <- combined_results %>%
  filter(strategy == "Full Kelly") %>%
  select(n_trades, median_wt, prob_loss) %>%
  arrange(n_trades)

message("\nFull Kelly Performance:")
for(i in 1:nrow(full_kelly_results)) {
  message(sprintf("  %5d trades: Median wealth = %.3f, P(loss) = %.1f%%",
                  full_kelly_results$n_trades[i],
                  full_kelly_results$median_wt[i],
                  full_kelly_results$prob_loss[i] * 100))
}

message("\nMonte Carlo simulation complete!")
message(sprintf("Total runtime: [check timestamp]"))
message(paste(rep("=", 70), collapse = ""), "\n")