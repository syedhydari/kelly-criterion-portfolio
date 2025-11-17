# kelly-criterion-portfolio
Replication of Carta &amp; Conversano (2020) Kelly Criterion study
# Kelly Criterion Portfolio Optimization

Replication and extension of Carta & Conversano (2020): "Practical Implementation of the Kelly Criterion: Optimal Growth Rate, Number of Trades, and Rebalancing Frequency for Equity Portfolios"

## Team Members
Aniqa Nayim, Agna Chan, Pranav Kasibhatla, Syed Bashir Hydari

## Project Overview

This project replicates the empirical study by Carta & Conversano (2020) that tests the Kelly Criterion for portfolio optimization using Euro Stoxx 50 data (2007-2018). We compare Kelly portfolios with traditional mean-variance portfolios across different:

- **Rebalancing frequencies:** daily, weekly, monthly, quarterly
- **Portfolio sizes:** 5, 10, 15, 20 stocks
- **Performance metrics:** CAGR, Sharpe ratio, Max Drawdown, VaR₉₉, ES₉₉

### Extensions
We extend the original paper by implementing regime-dependent Kelly fractions using Kim's (2024) Kelly Criterion Extension (KCE) to add dynamic market condition awareness.

## Project Structure
```
kelly-criterion-portfolio/
├── data/
│   ├── raw/              # Original downloaded data
│   └── processed/        # Cleaned data ready for analysis
├── code/
│   ├── 00_setup/         # Installation and configuration
│   ├── 01_data/          # Data collection scripts
│   ├── 02_optimization/  # Portfolio optimization
│   ├── 03_analysis/      # Performance analysis
│   └── 04_visualization/ # Plotting functions
├── output/
│   ├── figures/          # Generated plots
│   ├── tables/           # Results tables
│   └── reports/          # Final documents
└── docs/                 # Documentation and reports
```

## Setup Instructions

### Prerequisites
- R (version 4.0 or higher)
- RStudio or Positron
- Git

### 1. Clone Repository
```bash
git clone https://github.com/your-username/kelly-criterion-portfolio.git
cd kelly-criterion-portfolio
```

### 2. Install Required R Packages
Open R or Positron and run:
```r
source("code/00_setup/install_packages.R")
```

This will install all required packages including:
- `quantmod` - Financial data download
- `tidyverse` - Data manipulation
- `xts`, `zoo` - Time series
- `quadprog` - Quadratic programming for Kelly optimization
- `PerformanceAnalytics` - Portfolio metrics

### 3. Download Data
```r
source("code/01_data/01_download_data.R")
```

This downloads Euro Stoxx 50 daily prices from Yahoo Finance (2005-2018).

## Data

### Source
- **Index:** Euro Stoxx 50
- **Provider:** Yahoo Finance (via quantmod)
- **Period:** January 2007 - December 2018 (with 2005-2006 buffer for rolling estimation)
- **Frequency:** Daily adjusted close prices

### Tickers
Approximately 35 major constituents including:
- **Germany:** SAP, Siemens, Allianz, BASF, Daimler
- **France:** LVMH, L'Oréal, TotalEnergies, BNP Paribas, Airbus
- **Netherlands:** ASML, Philips, ING Group
- **Spain:** Santander, BBVA, Iberdrola, Inditex
- **Italy:** ENI, Intesa Sanpaolo, Enel

See `data/raw/stoxx50_tickers.csv` for complete list.

## Methodology

### Kelly Criterion
The Kelly Criterion maximizes the expected logarithmic growth rate of wealth:
```
maximize: μ'w - (1/2)w'Σw
subject to: Σw = 1, w ≥ 0
```

Where:
- `w` = portfolio weights
- `μ` = expected returns
- `Σ` = covariance matrix

### Estimation
- **Rolling window:** 24 months (≈504 trading days)
- **Rebalancing:** Monthly
- **Constraints:** Long-only, no leverage (weights sum to 1)

### Performance Metrics
- **CAGR:** Compound Annual Growth Rate
- **Sharpe Ratio:** Risk-adjusted returns
- **Max Drawdown:** Largest peak-to-trough decline
- **VaR₉₉:** Value at Risk (99% confidence)
- **ES₉₉:** Expected Shortfall (99% confidence)

## Timeline

| Week | Dates | Milestone |
|------|-------|-----------|
| 1 | Oct 28 - Nov 3 | Data Collection & Cleaning |
| 2 | Nov 4 - Nov 10 | Model Setup (Kelly & Mean-Variance) |
| 3 | Nov 11 - Nov 17 | Run Baseline Replication |
| 4 | Nov 18 - Nov 24 | Validation & Reconciliation |
| 5 | Nov 25 - Dec 1 | Develop Extensions |
| 6 | Dec 2 - Dec 8 | Analysis & Draft Report |
| 7 | Dec 9 - Dec 15 | Final Submission & Presentation |

## Checklist
A structured checklist for reproducing and extending the methodology, experiments, and results described in *"Practical Implementation of the Kelly Criterion."*  

---

### Methods

- [x] Optimization Methods
  - [x] Implement **Kelly Criterion theory**: continuous-time Kelly approximation Function (Anlytical)
  - [x] Implement **Modern Portfolio Theory**: Min-Variance/Markovitz (Anlytical)
  - [x] Implement **Equal Weights Calculation**
  - [x] Implement **Tangent Portfolio Opt** (Bashir)
- [x] Add **fractional Kelly scaling** parameter (e.g., ½-Kelly, ⅓-Kelly)
- [ ] Implement **simulation of GBM returns** for synthetic tests
- [x] Prepare **data ingestion** for EuroStoxx50 historical returns
- [x] Compute **risk metrics** (CAGR, volatility, drawdown, Sharpe, Sortino)

---

### Experiments

- [x] **Monte-Carlo simulations** (Aniqa/Agna) 
  - [x] Simulate 100, 1,000, 10,000, 40,000 trades
  - [x] Compare Half, Full, Double, and Triple Kelly
  - [x] Compute mean, median, std, and ruin probabilities
  - [x] Plot E[log W_T] over time (Figure 2)
  - [x] Results match Carta & Conversano (2020) Section 3.1
- [x] **Single-equity study** (Bashir)
  - [x] Use daily returns of one stock (e.g., Banca Intesa) (Bashir)
  - [x] Backtest Full, Double, Triple, Fractional Kelly (Bashir)
- [x] **Portfolio experiments** (Bashir)
  - [x] Construct in-sample constrained Kelly portfolio (Figure 5) (Bashir)
  - [x] Compare with tangent, min-variance, and equal-weight portfolios (Bashir)
- [x] Record all metrics in tabular format (mean, median, CAGR, drawdown)


---

### Results

- [ ] Visualize **log-wealth distributions** across Kelly multiples
- [ ] Plot **cumulative returns** for single-asset strategies
- [ ] Plot **cumulative returns** for In-Sample constrained Kelly
- [x] Plot **cumulative returns** for Rolling backtest 
- [ ] Draw **efficient frontier** with Kelly, tangent, and benchmark portfolios (Pranav)
- [ ] Chart **rolling portfolio performance** over time
- [ ] Summarize all results in tables:
  - [ ] Monte-Carlo statistics
  - [ ] Single-asset performance metrics
  - [ ] In-sample portfolio metrics
- [ ] Interpret:
  - [ ] Kelly asymptotic optimality
  - [ ] Over-betting risk and ruin probability
  - [ ] Portfolio concentration effects
  - [ ] Impact of window length and rebalancing frequency

---

### Extra Credit

- [x] Essay on challenges that thwarted 100% reproducibility (Bashir)

## Repository Structure

### Code Files
- `00_setup/install_packages.R` - Package installation
- `01_data/01_download_data.R` - Download price data
- `01_data/02_clean_data.R` - Data quality checks and cleaning
- `01_data/03_calculate_returns.R` - Compute log returns
- `02_model/01_returns_calculation.R` - Calculate Returns
- `02_model/02_optimization_functions.R` - Optimization Methods
- `02_model/03_rolling_backtest.R` - Rolling Backtest 
- `02_model/04_vizualization.R` - Visualization Functions
- `02_model/main.R` - Main file Reproducing Results


### Output Files
- `output/figures/` - Performance plots, drawdown charts
- `output/tables/` - Summary statistics, performance metrics
- `output/reports/` - Final report and presentation

---

## Monte Carlo Simulation Results

### Overview
Successfully replicated Section 3.1 of Carta & Conversano (2020) using Monte Carlo simulations to validate Kelly Criterion theoretical properties.

### Key Results
- **Full Kelly** achieved highest median wealth: **6.90x** at 10,000 trades
- **Triple Kelly** demonstrated danger of over-betting: **78.9%** probability of loss
- **Half Kelly** provided conservative alternative: **3.5%** probability of loss
- Results match paper with **>99% accuracy**

### Files
- `code/02_model/monte_carlo_simulation.R` - Main simulation engine
- `code/02_model/monte_carlo_visualizations.R` - Figure generation
- `output/tables/monte_carlo/` - Tables 1-4 (all scenarios)
- `output/figures/monte_carlo/` - Figure 2 and additional plots
- `MONTE_CARLO_RESULTS.md` - Detailed analysis and findings

### Key Findings
1. Full Kelly maximizes median wealth (theoretical optimum validated)
2. Over-betting (>f*) leads to high ruin probability
3. Under-betting (fractional Kelly) reduces risk but sacrifices growth
4. Long time horizons (10,000+ trades) required to see Kelly properties

See **MONTE_CARLO_RESULTS.md** for complete analysis.


## References

Carta, A., & Conversano, C. (2020). Practical Implementation of the Kelly Criterion: Optimal Growth Rate, Number of Trades, and Rebalancing Frequency for Equity Portfolios. *Frontiers in Applied Mathematics and Statistics*, 6, 577050. https://doi.org/10.3389/fams.2020.577050

Kim, S. (2024). Kelly Criterion Extension. *Mathematics*, 12(11), 1725. https://doi.org/10.3390/math12111725

## License

This project is for academic purposes only.

## Contact

For questions, contact: [cc5314@columbia.edu]
