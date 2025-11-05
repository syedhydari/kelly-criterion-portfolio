# Visualization for Monte Carlo Simulations
# Replicates Figure 2 from Carta & Conversano (2020)

library(tidyverse)
library(scales)
library(patchwork)

# Source the Monte Carlo simulation functions
source("code/02_model/monte_carlo_simulation.R")

# ============================================================================
# FIGURE 2: MEAN LOG-WEALTH CURVE
# ============================================================================

message("\n=== Creating Figure 2: Mean Log-Wealth vs Kelly Fraction ===\n")

# Calculate mean log-wealth for 10,000 trades
set.seed(123)
log_wealth_data <- calculate_mean_log_wealth(
  n_trades = 10000,
  n_trajectories = 10000,
  fractions = seq(0, 1.2, length.out = 100)
)

# Identify key Kelly fractions
kelly_points <- tibble(
  fraction = c(F_HALF_KELLY, F_FULL_KELLY, F_DOUBLE_KELLY, F_TRIPLE_KELLY),
  label = c("Half", "Full", "Double", "Triple"),
  color = c("#2ECC71", "#E74C3C", "#F39C12", "#9B59B6")
)

# Get corresponding log-wealth values
kelly_points <- kelly_points %>%
  mutate(
    mean_log_wealth = approx(log_wealth_data$fraction, 
                              log_wealth_data$mean_log_wealth, 
                              xout = fraction)$y
  )

# Create the plot (matching Figure 2 style)
p_log_wealth <- ggplot(log_wealth_data, aes(x = fraction, y = mean_log_wealth)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  
  # Mark Kelly fractions
  geom_vline(data = kelly_points, aes(xintercept = fraction, color = label),
             linetype = "dashed", linewidth = 0.8) +
  
  geom_point(data = kelly_points, 
             aes(x = fraction, y = mean_log_wealth, color = label),
             size = 4) +
  
  # Add labels
  geom_text(data = kelly_points,
            aes(x = fraction, y = mean_log_wealth, label = label, color = label),
            vjust = -1.5, size = 4, fontface = "bold", show.legend = FALSE) +
  
  scale_color_manual(values = setNames(kelly_points$color, kelly_points$label)) +
  
  labs(
    title = "Mean Log-Wealth of Each Fraction (10,000 trades)",
    subtitle = "Demonstrating optimal and sub-optimal Kelly fractions",
    x = "Fraction of Wealth Invested",
    y = "Mean Log-Wealth",
    color = "Kelly Strategy"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# Save
dir.create("output/figures/monte_carlo", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/monte_carlo/figure_2_mean_log_wealth.png", 
       p_log_wealth,
       width = 10, height = 7, dpi = 300)

message("Saved: output/figures/monte_carlo/figure_2_mean_log_wealth.png\n")

# ============================================================================
# ADDITIONAL VISUALIZATION: WEALTH DISTRIBUTION
# ============================================================================

message("=== Creating wealth distribution plots ===\n")

# Function to simulate and plot wealth distribution
plot_wealth_distribution <- function(n_trades, n_trajectories = 10000) {
  
  message(sprintf("  Simulating %d trades...", n_trades))
  
  strategies <- list(
    "Half Kelly" = F_HALF_KELLY,
    "Full Kelly" = F_FULL_KELLY,
    "Double Kelly" = F_DOUBLE_KELLY,
    "Triple Kelly" = F_TRIPLE_KELLY
  )
  
  wealth_data <- list()
  
  for(strategy_name in names(strategies)) {
    f <- strategies[[strategy_name]]
    
    final_wealths <- numeric(n_trajectories)
    
    for(i in 1:n_trajectories) {
      returns <- generate_gbm_returns(n_trades, MU_DAILY, SIGMA_DAILY)
      W <- simulate_wealth(1, f, returns, RF_DAILY)
      final_wealths[i] <- W[length(W)]
    }
    
    wealth_data[[strategy_name]] <- tibble(
      strategy = strategy_name,
      final_wealth = final_wealths
    )
  }
  
  wealth_df <- bind_rows(wealth_data)
  
  # Plot distribution
  p <- ggplot(wealth_df, aes(x = final_wealth, fill = strategy, color = strategy)) +
    geom_density(alpha = 0.3, linewidth = 1) +
    geom_vline(data = wealth_df %>% 
                 group_by(strategy) %>% 
                 summarise(median = median(final_wealth)),
               aes(xintercept = median, color = strategy),
               linetype = "dashed", linewidth = 1) +
    scale_x_log10(labels = number_format(accuracy = 0.1)) +
    scale_fill_manual(values = c("Half Kelly" = "#2ECC71",
                                   "Full Kelly" = "#E74C3C",
                                   "Double Kelly" = "#F39C12",
                                   "Triple Kelly" = "#9B59B6")) +
    scale_color_manual(values = c("Half Kelly" = "#2ECC71",
                                    "Full Kelly" = "#E74C3C",
                                    "Double Kelly" = "#F39C12",
                                    "Triple Kelly" = "#9B59B6")) +
    labs(
      title = sprintf("Final Wealth Distribution (%d trades)", n_trades),
      subtitle = "Dashed lines show median values",
      x = "Final Wealth (log scale)",
      y = "Density",
      fill = "Strategy",
      color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
  
  return(p)
}

# Create distributions for different scenarios
p_dist_100 <- plot_wealth_distribution(100)
p_dist_1000 <- plot_wealth_distribution(1000)
p_dist_10000 <- plot_wealth_distribution(10000)

# Combine
p_distributions <- (p_dist_100 | p_dist_1000) / p_dist_10000 +
  plot_annotation(
    title = "Wealth Distributions Across Kelly Strategies",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

ggsave("output/figures/monte_carlo/wealth_distributions.png",
       p_distributions,
       width = 14, height = 10, dpi = 300)

message("Saved: output/figures/monte_carlo/wealth_distributions.png\n")

# ============================================================================
# CUMULATIVE WEALTH PATHS (Sample Trajectories)
# ============================================================================

message("=== Creating sample wealth paths ===\n")

# Function to plot sample paths
plot_sample_paths <- function(n_trades, n_samples = 50) {
  
  set.seed(456)
  
  strategies <- list(
    "Half Kelly" = F_HALF_KELLY,
    "Full Kelly" = F_FULL_KELLY,
    "Double Kelly" = F_DOUBLE_KELLY,
    "Triple Kelly" = F_TRIPLE_KELLY
  )
  
  path_data <- list()
  
  for(strategy_name in names(strategies)) {
    f <- strategies[[strategy_name]]
    
    for(i in 1:n_samples) {
      returns <- generate_gbm_returns(n_trades, MU_DAILY, SIGMA_DAILY)
      W <- simulate_wealth(1, f, returns, RF_DAILY)
      
      path_data[[paste(strategy_name, i)]] <- tibble(
        strategy = strategy_name,
        trajectory = i,
        period = 0:n_trades,
        wealth = W
      )
    }
  }
  
  paths_df <- bind_rows(path_data)
  
  p <- ggplot(paths_df, aes(x = period, y = wealth, 
                             group = interaction(strategy, trajectory),
                             color = strategy)) +
    geom_line(alpha = 0.3, linewidth = 0.5) +
    facet_wrap(~ strategy, ncol = 2, scales = "free_y") +
    scale_y_log10(labels = number_format(accuracy = 0.1)) +
    scale_color_manual(values = c("Half Kelly" = "#2ECC71",
                                    "Full Kelly" = "#E74C3C",
                                    "Double Kelly" = "#F39C12",
                                    "Triple Kelly" = "#9B59B6")) +
    labs(
      title = sprintf("Sample Wealth Paths (%d trades, %d trajectories each)", 
                      n_trades, n_samples),
      x = "Trading Period",
      y = "Wealth (log scale)",
      color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 12)
    )
  
  return(p)
}

p_paths_1000 <- plot_sample_paths(1000, n_samples = 30)
p_paths_10000 <- plot_sample_paths(10000, n_samples = 20)

ggsave("output/figures/monte_carlo/sample_paths_1000.png",
       p_paths_1000,
       width = 12, height = 8, dpi = 300)

ggsave("output/figures/monte_carlo/sample_paths_10000.png",
       p_paths_10000,
       width = 12, height = 8, dpi = 300)

message("Saved: output/figures/monte_carlo/sample_paths_1000.png")
message("Saved: output/figures/monte_carlo/sample_paths_10000.png\n")

# ============================================================================
# SUMMARY VISUALIZATION: KEY METRICS ACROSS SCENARIOS
# ============================================================================

message("=== Creating summary comparison plots ===\n")

# Load saved simulation results
all_sims <- read_csv("output/tables/monte_carlo/all_simulations.csv", 
                     show_col_types = FALSE)

# Plot 1: Median wealth across scenarios
p_median <- ggplot(all_sims, aes(x = as.factor(n_trades), y = median_wt, 
                                  fill = strategy, group = strategy)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%.2f", median_wt)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Half Kelly" = "#2ECC71",
                                "Full Kelly" = "#E74C3C",
                                "Double Kelly" = "#F39C12",
                                "Triple Kelly" = "#9B59B6")) +
  labs(
    title = "Median Final Wealth by Number of Trades",
    x = "Number of Trades",
    y = "Median Final Wealth",
    fill = "Strategy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Plot 2: Probability of loss
p_loss <- ggplot(all_sims, aes(x = as.factor(n_trades), y = prob_loss * 100,
                                 fill = strategy, group = strategy)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%.1f%%", prob_loss * 100)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Half Kelly" = "#2ECC71",
                                "Full Kelly" = "#E74C3C",
                                "Double Kelly" = "#F39C12",
                                "Triple Kelly" = "#9B59B6")) +
  labs(
    title = "Probability of Loss by Number of Trades",
    x = "Number of Trades",
    y = "P(Final Wealth < Initial) %",
    fill = "Strategy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Combine
p_summary <- p_median / p_loss +
  plot_annotation(
    title = "Kelly Strategy Performance Summary",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

ggsave("output/figures/monte_carlo/summary_comparison.png",
       p_summary,
       width = 12, height = 10, dpi = 300)

message("Saved: output/figures/monte_carlo/summary_comparison.png\n")

# ============================================================================
# FINAL MESSAGE
# ============================================================================

message(paste(rep("=", 70), collapse = ""))
message("ALL MONTE CARLO VISUALIZATIONS COMPLETE!")
message(paste(rep("=", 70), collapse = ""))
message("\nGenerated files:")
message("  - figure_2_mean_log_wealth.png (Main paper figure)")
message("  - wealth_distributions.png")
message("  - sample_paths_1000.png")
message("  - sample_paths_10000.png")
message("  - summary_comparison.png")
message("\nAll files saved to: output/figures/monte_carlo/")
message(paste(rep("=", 70), collapse = ""), "\n")