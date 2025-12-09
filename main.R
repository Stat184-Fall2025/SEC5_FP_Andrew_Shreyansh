# main.R ----------------------------------------------------------------
# Project: Multi-dataset R momentum + backtesting (Data Ingest + EDA stage)
# Goal: Pull asset + macro data, tidy it, and create clear, figures comparing
# different strategies

# -----------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------
library(tidyverse)
library(tidyquant)

source("functions.R")  # uses get_price_data(), get_macro_data(), etc.

# -----------------------------------------------------------------------
# 1. Parameters 
# -----------------------------------------------------------------------
start_date <- as.Date("2024-01-01")
end_date   <- Sys.Date()

# Might need to include other asssests such as gold and crypto
# Asset tickers (equity focus)
tickers <- c("SPY", "NVDA", "UNH", "RKLB", "META")

# -----------------------------------------------------------------------
# 2. Data ingest
# -----------------------------------------------------------------------

# 2a. Asset price data from Yahoo Finance
prices_raw <- get_price_data(
  symbols = tickers,
  start   = start_date,
  end     = end_date
)

# 2b. Macro / volatility data (VIX + 3m T-bill)
macro_raw <- get_macro_data(
  start = start_date,
  end   = end_date
)

# -----------------------------------------------------------------------
# 3. Tidying & joining
# -----------------------------------------------------------------------

# 3a. Tidy asset prices
prices_tidy <- tidy_price_data(prices_raw)
# Columns: symbol, date, price

# 3b. Tidy macro data
macro_tidy  <- tidy_macro_data(macro_raw)
# Columns: date, vix, tbill_3m

# 3c. Combine into one dataset: asset prices + macro features
data_all <- combine_price_macro(prices_tidy, macro_tidy)

# 3d. Add daily returns (for later analysis / plots)
data_all <- add_daily_returns(data_all)
# Columns now: symbol, date, price, vix, tbill_3m, daily_return

glimpse(data_all)

# (Optional) could save processed data for reproducibility
# dir.create("data", showWarnings = FALSE)
# write_csv(data_all, file = "data/data_all.csv")

# -----------------------------------------------------------------------
# 4. Visualizations 
# -----------------------------------------------------------------------
# NOTE: Moving to a Quarto (.qmd) document, each plot here can be
#       included as a figure with:
#       - A chunk label (e.g., fig-prices)
#       - A figure caption describing key patterns
#       - Alt text (for accessibility) describing what the figure shows.

# -----------------------------------------------------------------------
# Figure 1: Asset prices over time (log scale)
# -----------------------------------------------------------------------
# Purpose:
# - Compare growth and volatility across SPY, NVDA, UNH, RKLB, META.
# - Log scale helps compare relative changes when prices differ in level.
# - essentially normalizes the slope

p_prices <- data_all %>%
  ggplot(aes(x = date,
             y = price,
             color = symbol,
             linetype = symbol)) +
  geom_line(linewidth = 0.6) +
  scale_y_log10() +
  labs(
    title = "Daily Adjusted Closing Prices (Log Scale)",
    subtitle = "Equity tickers: SPY, NVDA, UNH, RKLB, META",
    x = "Date",
    y = "Adjusted closing price (USD, log10 scale)",
    color = "Ticker",
    linetype = "Ticker"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "right"
  )

print(p_prices)

# alt text for Quarto later:
# "Line plot of daily adjusted closing prices for SPY, NVDA, UNH, RKLB, and META
#  from January 2025 to present on a log10 scale, showing differing growth and volatility."

# -----------------------------------------------------------------------
# Figure 2: Market volatility over time (VIX)
# -----------------------------------------------------------------------
# Purpose:
# - Show how overall market volatility evolves over the sample.
# - Gives context for periods where asset returns may be more extreme.

p_vix <- macro_tidy %>%
  ggplot(aes(x = date, y = vix)) +
  geom_line(linewidth = 0.6) +
  labs(
    title = "Market Volatility Over Time",
    subtitle = "CBOE Volatility Index (VIX)",
    x = "Date",
    y = "VIX level (index points)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(p_vix)

# alt text for Quarto:
# "Line plot of the VIX index over time, with higher spikes indicating periods
#  of elevated market volatility."

# -----------------------------------------------------------------------
# Figure 3: Distribution of daily returns for SPY
# -----------------------------------------------------------------------
# Purpose:
# - Explore the typical size of daily moves in SPY.
# - Possibly connect to volatility and risk in a simple way.

p_returns_spy <- data_all %>%
  filter(symbol == "SPY",
         !is.na(daily_return)) %>%
  ggplot(aes(x = daily_return)) +
  geom_histogram(
    bins  = 40,
    color = "white"
  ) +
  labs(
    title = "Distribution of Daily Returns – SPY",
    subtitle = "Daily log returns implied by adjusted prices",
    x = "Daily return (proportion)",
    y = "Number of trading days"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(p_returns_spy)

# Possible alt text for Quarto:
# "Histogram of daily returns for SPY, showing a distribution centered near zero
#  with a few large positive and negative outliers."

# -----------------------------------------------------------------------
# 5. (Optional) Save plots for use 
# -----------------------------------------------------------------------
# dir.create("figures", showWarnings = FALSE)
# ggsave("figures/fig1_prices_log.png",   p_prices,       width = 9, height = 5, dpi = 300)
# ggsave("figures/fig2_vix.png",         p_vix,          width = 9, height = 4, dpi = 300)
# ggsave("figures/fig3_spy_returns.png", p_returns_spy,  width = 7, height = 4, dpi = 300)

# -----------------------------------------------------------------------
# Continuation
# -----------------------------------------------------------------------
# From here, the next steps in the project will be:
# - Use `data_all` to compute momentum signals (e.g., moving averages, lookback returns)
# - Build backtesting functions that convert signals into positions and equity curves
# - Add new figures that compare strategy performance over time.
