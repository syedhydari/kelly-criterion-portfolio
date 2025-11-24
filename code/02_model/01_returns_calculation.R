# Convert wide format price data to tidy format with returns

library(tidyverse)
library(lubridate)
# options(pillar.sigfig = 8)


# Function to load and prepare data
prepare_kelly_data <- function(price_file_path) {
  
  # Read wide format price data
  prices_wide <- read_csv(price_file_path, show_col_types = FALSE)
  
  message(sprintf("Loaded data with %d rows and %d columns", 
                  nrow(prices_wide), ncol(prices_wide)))
  
  # Detect and rename date column
  date_col <- names(prices_wide)[1]
  prices_wide <- prices_wide %>%
    rename(DATE = all_of(date_col)) %>%
    mutate(DATE = ymd(DATE)) %>%
    arrange(DATE)
  
  message(sprintf("Date range: %s to %s", min(prices_wide$DATE), max(prices_wide$DATE)))
  
  # Clean column names (remove suffixes)
  # names(prices_wide)[-1] <- str_remove(names(prices_wide)[-1], "\\.(DE|MI|BR|AS|PA|MC)$|_DOUBLE$")

  # print(prices_wide)
  # sapply(prices_wide, class)

  
  message(sprintf("Number of tickers: %d", ncol(prices_wide) - 1))

  # x <- prices_wide$ABI.BR
  # print(head(x))
  # print(head(x / dplyr::lag(x) - 1))

  
  # Calculate returns in wide format
  returns_wide <- prices_wide %>%
    arrange(DATE) %>%
    mutate(across(where(is.numeric), ~ (.x / dplyr::lag(.x) - 1)), .names = "{.col}")

  
  
  # Remove first row (will be all NA)
  returns_wide <- returns_wide %>% slice(-1) 
  returns_wide <- returns_wide %>% select(-.names)


  print(returns_wide)
  
  message(sprintf("Calculated returns: %d periods", nrow(returns_wide)))
  
  # Convert returns to long format
  returns_long <- returns_wide %>%
    pivot_longer(
      cols = -DATE,
      names_to = "ticker",
      values_to = "return"
    ) #%>%
    #filter(!is.na(return), !is.infinite(return))

  print(returns_long)

  # Also keep prices in long format for reference
  prices_long <- prices_wide %>%
    pivot_longer(
      cols = -DATE,
      names_to = "ticker", 
      values_to = "price"
    ) #%>%
    #filter(!is.na(price), price > 0)
  
  # Add log returns to returns_long
  # returns_long <- returns_long %>%
  #   mutate(log_return = log(1 + return)) |> 
  #   filter(!is.na(log_return), !is.infinite(log_return))
  
  returns_long <- returns_long %>%
    mutate(
      log_return = ifelse(return <= -1, NA_real_, log(1 + return))
    )
  
  # Summary statistics
  summary_stats <- returns_long %>%
    group_by(ticker) %>%
    summarise(
      mean_return = mean(return, na.rm = TRUE),
      sd_return = sd(return, na.rm = TRUE),
      min_return = min(return, na.rm = TRUE),
      max_return = max(return, na.rm = TRUE),
      n_obs = sum(!is.na(return)),
      .groups = "drop"
    ) %>%
    mutate(
      annualized_return = mean_return * 252,  # Assuming daily data
      annualized_vol = sd_return * sqrt(252)
    )
  
  # Quality check
  message("\n=== Data Quality ===")
  message(sprintf("Unique tickers: %d", n_distinct(returns_long$ticker)))
  message(sprintf("Average observations per ticker: %.0f", 
                  mean(summary_stats$n_obs)))
  message(sprintf("Mean return range: %.4f%% to %.4f%%", 
                  min(summary_stats$mean_return) * 100,
                  max(summary_stats$mean_return) * 100))
  
  # Show sample
  message("\n=== Sample Returns (first ticker, first 5 days) ===")
  sample_data <- returns_long %>%
    arrange(ticker, DATE) %>%
    group_by(ticker) %>%
    slice(1:5) %>%
    ungroup() %>%
    head(5)
  print(sample_data)
  
  # Return list with all data formats
  list(
    prices_wide = prices_wide,
    prices_long = prices_long,
    returns_long = returns_long,
    returns_wide = returns_wide,
    summary_stats = summary_stats
  )
}

# Function to aggregate returns to different frequencies
aggregate_returns <- function(returns_long, frequency = c("daily", "weekly", "monthly")) {
  
  frequency <- match.arg(frequency)
  
  if (frequency == "daily") {
    return(returns_long)
  }
  
  returns_agg <- returns_long %>%
    mutate(
      period = case_when(
        frequency == "weekly" ~ floor_date(DATE, "week"),
        frequency == "monthly" ~ floor_date(DATE, "month")
      )
    ) %>%
    group_by(ticker, period) %>%
    summarise(
      DATE = last(DATE),
      # Compound returns
      return = prod(1 + return) - 1,
      log_return = sum(log_return),
      .groups = "drop"
    ) %>%
    select(DATE, ticker, return, log_return)
  
  return(returns_agg)
}

# Example usage:
# data <- prepare_kelly_data("euro_stoxx50_prices.csv")
# monthly_returns <- aggregate_returns(data$returns_long, "monthly")

# Data quality check function
check_data_quality <- function(returns_wide) {
  
  message("\n=== Data Quality Check ===")
  
  # Check date range
  date_range <- range(returns_wide$DATE)
  n_periods <- nrow(returns_wide)
  message(sprintf("Date range: %s to %s (%d periods)", 
                  date_range[1], date_range[2], n_periods))
  
  # Check for missing values
  ret_mat <- returns_wide %>% select(-DATE)
  na_by_col <- colSums(is.na(ret_mat))
  pct_complete <- 1 - na_by_col / nrow(ret_mat)
  
  message(sprintf("Tickers: %d", ncol(ret_mat)))
  message(sprintf("Complete data rate: %.1f%% (median across tickers)", 
                  median(pct_complete) * 100))
  message(sprintf("Tickers with >95%% complete data: %d", 
                  sum(pct_complete > 0.95)))
  
  # Check for extreme values
  extreme_returns <- ret_mat %>%
    as.matrix() %>%
    abs() %>%
    as.vector() %>%
    .[!is.na(.)] %>%
    quantile(0.999)
  
  message(sprintf("99.9th percentile absolute return: %.2f%%", 
                  extreme_returns * 100))
  
  # Recommend filtering
  if (median(pct_complete) < 0.8) {
    warning("Low data completeness. Consider filtering date range or tickers.")
  }
  
  invisible(list(
    date_range = date_range,
    n_periods = n_periods,
    pct_complete = pct_complete,
    extreme_return = extreme_returns
  ))
}