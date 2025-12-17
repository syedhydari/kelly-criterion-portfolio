# Kelly Criterion Portfolio Optimization

A comprehensive study implementing the Kelly Criterion for optimal portfolio allocation, featuring Monte Carlo simulations, backtesting strategies, and out-of-sample validation on major equity indices.

## Overview

The Kelly Criterion is a mathematical formula for optimal bet sizing that maximizes long-term geometric growth rate. This project applies the Kelly framework to portfolio optimization, comparing Full Kelly, Half Kelly, and traditional mean-variance strategies across multiple market indices.

## Repository Structure

```
kelly-criterion-portfolio/
├── Implementation/           # Core R code and analysis
│   ├── monte_carlo_simulation.R
│   ├── monte_carlo_visualizations.qmd
│   ├── in_sample_kelly_replication (eurostoxx 50).rmd
│   ├── out_of_sample_DJI.qmd
│   ├── out_of_sample_euro.qmd
│   ├── single_equity_kelly_backtest (banca intesa).rmd
│   ├── data/                 # Input datasets
│   └── output/               # Generated figures
│
├── Final_Report/             # Academic paper and figures
│   ├── QRM_Final_Project_Paper.qmd
│   └── QRM_Final_Project_Report.pdf
│
├── kelly_presentation/       # Beamer presentation slides
│   └── kelly_presentation.qmd
│
└── Extras/                   # Supplementary materials
```

## Tech Stack

- **R/Python** — Statistical computing
- **Quarto / R Markdown** — Reproducible documents
- **LaTeX / Beamer** — Presentation formatting

## Authors

- [Syed Bashir Hydari](https://github.com/syedhydari) | Agna Chan | Pranav Kasibhatla | Aniqa Nayim

## Acknowledgments

- Professor Zhuo for consultation and OOS replication on Dow Jones
- Carta, A., & Conversano, C. (2020). Practical implementation of the Kelly criterion

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.
