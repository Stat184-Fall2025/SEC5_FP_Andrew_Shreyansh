# Stock Strategy Profit Tracking
This repo contains all relevant code and reproducible files for our project for STAT 184 involving the tracking of profit/loss across different trading strategies

## Overview

This project explores how different stocks and market conditions behave over time and evaluates whether a simple technical trading rule can outperform a buy-and-hold benchmark. Using daily market data, we clean and combine asset prices with macroeconomic indicators, visualize key patterns, and test a 50-day simple moving average (SMA) momentum strategy against holding SPY.

The primary goals are to understand market behavior through exploratory data analysis (EDA), build reproducible data pipelines, and assess the risk-return tradeoffs of a basic systematic trading strategy.

### Interesting Insight

One notable insight from the project is that the 50-day SMA momentum strategy achieved higher risk-adjusted performance than buy-and-hold SPY, despite being invested only part of the time. The strategy produced a higher Sharpe ratio and a smaller maximum drawdown, suggesting better downside risk control during volatile periods.

A key visualization supporting this insight is the equity curve comparison, which shows the growth of $1 invested in the momentum strategy versus buy-and-hold SPY. This plot highlights how timing exposure based on trends can meaningfully change performance outcomes.

<img width="567" height="271" alt="Screenshot 2025-12-17 at 8 35 31 PM" src="https://github.com/user-attachments/assets/b7dec131-6e49-4a9b-b13f-9abfe5981270" />

## Data Sources and Acknowledgements

All data used in this project comes from publicly available sources:

- Stock and ETF price data: Yahoo Finance, accessed via the tidyquant R package

- Market volatility data: CBOE Volatility Index (VIX), via Yahoo Finance

- Interest rate data: 3-Month Treasury Bill Rate (TB3MS), from FRED

We also acknowledge the open-source R ecosystem, particularly tidyverse, ggplot2, tidyquant, and related packages, which make reproducible financial analysis possible.

This project is for educational purposes only and does not constitute investment advice.

## Current Plan

This project currently covers data ingestion, exploratory analysis, and one baseline momentum strategy. Future extensions may include:

- Testing alternative momentum windows and technical indicators

- Adding transaction costs and realistic execution assumptions

- Comparing multiple strategies across different volatility regimes

- Expanding the analysis to additional assets or longer time horizons
  
## Repo Structure

This repository is organized to support a fully reproducible Quarto-based statistical analysis.

- QuartoProj.qmd – The main Quarto document containing the complete analysis, written narrative, figures, and results. This file is used to generate the final PDF report included in the project.

- main.R – The primary R script that runs the core workflow of the project, including data ingestion, data cleaning, exploratory data analysis, and implementation of the trading strategy.

- functions.R – A collection of helper functions used throughout the project for tasks such as pulling financial data, transforming datasets, calculating returns, and constructing the momentum strategy. Separating these functions improves readability and reusability.

- README.md – Provides a high-level overview of the project’s purpose, data sources, structure, and authorship.

## Authors

- Andrew Hoang: ath5428@psu.edu
- Shreyansh Agarwal: sxa6086@psu.edu
