# ============================================================================
# Download Euro Stoxx 50 Historical Price Data
# ============================================================================
# Purpose: Download daily adjusted close prices for Euro Stoxx 50 constituents
# Data Period: 2005-01-01 to 2018-12-31 (includes buffer for rolling windows)
# Date: October 26, 2025
# ============================================================================

# Clear environment
rm(list = ls())

# Load required libraries
library(quantmod)
library(xts)
library(tidyverse)

cat("=======================================================\n")
cat("Euro Stoxx 50 Data Download Script\n")
cat("=======================================================\n\n")

# Set date range
start_date <- "2005-01-01"  # Start earlier for 24-month rolling window
end_date <- "2018-12-31"
analysis_start <- "2007-01-01"  # Actual analysis period

cat("Download period:", start_date, "to", end_date, "\n")
cat("Analysis period:", analysis_start, "to", end_date, "\n\n")

# ============================================================================
# Define Euro Stoxx 50 Tickers
# ============================================================================
# Note: Major constituents that existed throughout 2007-2018
# Yahoo Finance ticker format: SYMBOL.EXCHANGE

stoxx50_tickers <- c(
  # Germany (Frankfurt - .DE)
  "SAP.DE",      # SAP SE
  "SIE.DE",      # Siemens AG
  "ALV.DE",      # Allianz SE
  "BAS.DE",      # BASF SE
  "DAI.DE",      # Daimler AG
  "VOW3.DE",     # Volkswagen AG
  "MUV2.DE",     # Munich Re
  "DB1.DE",      # Deutsche Boerse
  "BMW.DE",      # BMW
  "DTE.DE",      # Deutsche Telekom
  
  # France (Euronext Paris - .PA)
  "MC.PA",       # LVMH
  "OR.PA",       # L'Oréal
  "SAN.PA",      # Sanofi
  "TTE.PA",      # TotalEnergies
  "BNP.PA",      # BNP Paribas
  "AIR.PA",      # Airbus
  "AI.PA",       # Air Liquide
  "SU.PA",       # Schneider Electric
  "CS.PA",       # AXA
  "BN.PA",       # Danone
  "VIE.PA",      # Veolia
  
  # Netherlands (Euronext Amsterdam - .AS)
  "ASML.AS",     # ASML Holding
  "PHIA.AS",     # Koninklijke Philips
  "INGA.AS",     # ING Groep
  "AD.AS",       # Ahold Delhaize
  "HEIA.AS",     # Heineken
  
  # Spain (Madrid - .MC)
  "SAN.MC",      # Banco Santander
  "BBVA.MC",     # BBVA
  "IBE.MC",      # Iberdrola
  "ITX.MC",      # Inditex
  "TEF.MC",      # Telefonica
  
  # Italy (Milan - .MI)
  "ENI.MI",      # ENI SpA
  "ISP.MI",      # Intesa Sanpaolo
  "UCG.MI",      # UniCredit
  "ENEL.MI",     # Enel SpA
  
  # Belgium (Brussels - .BR)
  "ABI.BR"       # Anheuser-Busch InBev
)

cat("Number of tickers to download:", length(stoxx50_tickers), "\n\n")

# Create ticker reference dataframe
ticker_df <- data.frame(
  ticker = stoxx50_tickers,
  company = c(
    # Germany
    "SAP", "Siemens", "Allianz", "BASF", "Daimler", "Volkswagen", 
    "Munich Re", "Deutsche Boerse", "BMW", "Deutsche Telekom",
    # France
    "LVMH", "L'Oreal", "Sanofi", "TotalEnergies", "BNP Paribas", 
    "Airbus", "Air Liquide", "Schneider Electric", "AXA", "Danone", "Veolia",
    # Netherlands
    "ASML", "Philips", "ING Group", "Ahold Delhaize", "Heineken",
    # Spain
    "Santander", "BBVA", "Iberdrola", "Inditex", "Telefonica",
    # Italy
    "ENI", "Intesa Sanpaolo", "UniCredit", "Enel",
    # Belgium
    "AB InBev"
  ),
  country = c(
    rep("Germany", 10), 
    rep("France", 11), 
    rep("Netherlands", 5),
    rep("Spain", 5), 
    rep("Italy", 4), 
    rep("Belgium", 1)
  ),
  stringsAsFactors = FALSE
)

# Save ticker list
write.csv(ticker_df, "data/raw/stoxx50_tickers.csv", row.names = FALSE)
cat("✓ Ticker list saved to: data/raw/stoxx50_tickers.csv\n\n")

# ============================================================================
# Download Function with Error Handling
# ============================================================================

download_stock_data <- function(ticker, start, end) {
  tryCatch({
    cat("Downloading:", ticker, "... ")
    
    # Download data
    data <- getSymbols(ticker, 
                      from = start,
                      to = end,
                      auto.assign = FALSE,
                      warnings = FALSE)
    
    # Extract adjusted close
    adj_close <- Ad(data)
    colnames(adj_close) <- ticker
    
    cat("✓ Success (", nrow(adj_close), "observations)\n", sep = "")
    
    # Small delay to avoid overwhelming API
    Sys.sleep(0.5)
    
    return(adj_close)
    
  }, error = function(e) {
    cat("✗ Failed -", conditionMessage(e), "\n")
    return(NULL)
  })
}

# ============================================================================
# Download All Tickers
# ============================================================================

cat("Starting download from Yahoo Finance...\n")
cat("This may take 5-10 minutes. Please be patient.\n")
cat("=======================================================\n\n")

price_list <- list()
failed_tickers <- c()

# Progress tracking
total_tickers <- length(stoxx50_tickers)
completed <- 0

for(ticker in stoxx50_tickers) {
  completed <- completed + 1
  cat(sprintf("[%d/%d] ", completed, total_tickers))
  
  result <- download_stock_data(ticker, start_date, end_date)
  
  if(!is.null(result)) {
    price_list[[ticker]] <- result
  } else {
    failed_tickers <- c(failed_tickers, ticker)
  }
}

cat("\n=======================================================\n")
cat("Download Summary:\n")
cat("=======================================================\n")
cat("  Total tickers attempted:", length(stoxx50_tickers), "\n")
cat("  Successfully downloaded:", length(price_list), "\n")
cat("  Failed downloads:", length(failed_tickers), "\n")
cat("  Success rate:", 
    round(length(price_list)/length(stoxx50_tickers)*100, 1), "%\n")

if(length(failed_tickers) > 0) {
  cat("\nFailed tickers:\n")
  for(ticker in failed_tickers) {
    cat("  -", ticker, "\n")
  }
}

# ============================================================================
# Merge and Save Price Data
# ============================================================================

if(length(price_list) > 0) {
  cat("\n=======================================================\n")
  cat("Processing Downloaded Data\n")
  cat("=======================================================\n\n")
  
  cat("Merging price data...\n")
  
  # Merge all xts objects
  all_prices <- do.call(merge, price_list)
  
  cat("✓ Price data merged successfully\n\n")
  
  # Print data summary
  cat("Data Summary:\n")
  cat("  Stocks:", ncol(all_prices), "\n")
  cat("  Observations:", nrow(all_prices), "\n")
  cat("  Start date:", as.character(start(all_prices)), "\n")
  cat("  End date:", as.character(end(all_prices)), "\n")
  cat("  Total data points:", nrow(all_prices) * ncol(all_prices), "\n\n")
  
  # Check for missing values
  na_count <- sum(is.na(all_prices))
  na_pct <- na_count / (nrow(all_prices) * ncol(all_prices)) * 100
  
  cat("Missing Values:\n")
  cat("  Total NAs:", na_count, "\n")
  cat("  Percentage:", round(na_pct, 2), "%\n\n")
  
  # Save raw price data
  cat("Saving data...\n")
  saveRDS(all_prices, "data/raw/euro_stoxx50_prices.rds")
  cat("✓ Raw prices saved to: data/raw/euro_stoxx50_prices.rds\n")
  
  # Save as CSV for inspection (optional)
  write.zoo(all_prices, "data/raw/euro_stoxx50_prices.csv", sep = ",")
  cat("✓ CSV backup saved to: data/raw/euro_stoxx50_prices.csv\n")
  
  # Save failed tickers if any
  if(length(failed_tickers) > 0) {
    write.csv(data.frame(ticker = failed_tickers), 
              "data/raw/failed_tickers.csv", 
              row.names = FALSE)
    cat("✓ Failed tickers saved to: data/raw/failed_tickers.csv\n")
  }
  
  # Quick data quality check
  cat("\n=======================================================\n")
  cat("Quick Data Quality Check\n")
  cat("=======================================================\n\n")
  
  # Show first few rows
  cat("First 5 rows (first 5 stocks):\n")
  print(head(all_prices[, 1:min(5, ncol(all_prices))], 5))
  
  cat("\nLast 5 rows (first 5 stocks):\n")
  print(tail(all_prices[, 1:min(5, ncol(all_prices))], 5))
  
  # Summary statistics
  cat("\nPrice ranges by stock:\n")
  price_summary <- data.frame(
    ticker = colnames(all_prices),
    min_price = apply(all_prices, 2, min, na.rm = TRUE),
    max_price = apply(all_prices, 2, max, na.rm = TRUE),
    mean_price = apply(all_prices, 2, mean, na.rm = TRUE),
    na_count = apply(all_prices, 2, function(x) sum(is.na(x)))
  )
  
  print(head(price_summary, 10))
  cat("... (showing first 10 stocks)\n")
  
  cat("\n=======================================================\n")
  cat("Data Download Complete!\n")
  cat("=======================================================\n\n")
  
  cat("Next steps:\n")
  cat("1. Run data cleaning script: source('code/01_data/02_clean_data.R')\n")
  cat("2. Inspect data quality report in data/processed/\n")
  cat("3. Calculate returns: source('code/01_data/03_calculate_returns.R')\n\n")
  
} else {
  cat("\n=======================================================\n")
  cat("ERROR: No Data Downloaded\n")
  cat("=======================================================\n\n")
  cat("No data was downloaded successfully.\n")
  cat("Possible issues:\n")
  cat("1. Internet connection problems\n")
  cat("2. Yahoo Finance API issues\n")
  cat("3. Incorrect ticker symbols\n")
  cat("4. Firewall blocking access\n\n")
  cat("Please check your connection and try again.\n")
}

# Print completion message
cat("Script execution completed at:", as.character(Sys.time()), "\n")