library(tidyverse)
library(tidyquant)

source("functions.R")

start_date <- as.Date("2025-01-01")
end_date   <- Sys.Date()

# ----------------------------------------------------------------------
# 1. Parameters
# ----------------------------------------------------------------------

# pulling data related to asset pricing
tickers <- c("SPY", "NVDA", "UNH", "RKLB", "META")
asset_prices <- tq_get(tickers, 
                       get = "stock.prices",
                       from = start_date,
                       to = end_date)

# ----------------------------------------------------------------------
# 2. Pull raw data from both sources
# ----------------------------------------------------------------------
prices_raw <- get_price_data(
  symbols = tickers,
  start   = start_date,
  end     = end_date
)

macro_raw <- get_macro_data(
  start = start_date,
  end   = end_date
)

# ----------------------------------------------------------------------
# 3. Tidy each dataset
# ----------------------------------------------------------------------
prices_tidy <- tidy_price_data(prices_raw)
macro_tidy  <- tidy_macro_data(macro_raw)

# Combine into one dataset: prices + VIX + TB3MS
data_all <- combine_price_macro(prices_tidy, macro_tidy)

# Optional: add daily returns (for later steps)
data_all <- add_daily_returns(data_all)

# Quick sanity check in console
glimpse(data_all)

# ----------------------------------------------------------------------
# 4. Simple visualizations with ggplot2
# ----------------------------------------------------------------------

# (a) Price paths for all tickers, faceted by symbol
p_prices <- data_all %>%
  ggplot(aes(x = date, y = price, color = symbol)) +
  geom_line() +
  facet_wrap(~ symbol, scales = "free_y") +
  labs(
    title = "Asset Prices Over Time",
    x = "Date",
    y = "Adjusted Price"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_prices)

# (b) VIX over time (macro view)
p_vix <- macro_tidy %>%
  ggplot(aes(x = date, y = vix)) +
  geom_line() +
  labs(
    title = "VIX (Market Volatility) Over Time",
    x = "Date",
    y = "VIX Level"
  ) +
  theme_minimal()

print(p_vix)

# (c) Optional: daily returns distribution for one ticker (e.g. SPY)
p_returns_spy <- data_all %>%
  filter(symbol == "SPY", !is.na(daily_return)) %>%
  ggplot(aes(x = daily_return)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Daily Returns – SPY",
    x = "Daily Return",
    y = "Count"
  ) +
  theme_minimal()

print(p_returns_spy)

# You can later save these for your report/check-in:
# ggsave("figures/prices_by_symbol.png", p_prices, width = 9, height = 5)
# ggsave("figures/vix_over_time.png",    p_vix,    width = 9, height = 4)
# ggsave("figures/spy_return_hist.png",  p_returns_spy, width = 7, height = 4)





