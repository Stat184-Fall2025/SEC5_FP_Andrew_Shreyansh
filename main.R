# main.R ----------------------------------------------------------------
# Project: Multi-dataset R momentum + backtesting
# Stage: Data Ingest, EDA, and First Strategy Backtest
# Goal: Pull asset + macro data, tidy it, and create clear figures and a
#       first strategy comparison (50-day SMA vs SPY buy-and-hold)
# -----------------------------------------------------------------------

# 0. Setup --------------------------------------------------------------
library(tidyverse)
library(tidyquant)
source("functions.R")  # uses get_price_data(), get_macro_data(), etc.

# 1. Parameters ---------------------------------------------------------
start_date <- as.Date("2024-01-01")
end_date   <- Sys.Date()

# Asset tickers (equity focus)
# Might later expand to include gold, crypto, sector ETFs, etc.
tickers <- c("SPY", "NVDA", "UNH", "RKLB", "META")

# 2. Data ingest --------------------------------------------------------
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

# 3. Tidying & joining --------------------------------------------------
# 3a. Tidy asset prices
prices_tidy <- tidy_price_data(prices_raw)
# Columns: symbol, date, price

# 3b. Tidy macro data
macro_tidy  <- tidy_macro_data(macro_raw)
# Columns: date, vix, tbill_3m

# 3c. Combine into one dataset: asset prices + macro features
data_all <- combine_price_macro(prices_tidy, macro_tidy)

# 3d. Add daily returns (for later analysis / plots and strategies)
data_all <- add_daily_returns(data_all)
# Columns now: symbol, date, price, vix, tbill_3m, daily_return

glimpse(data_all)

# (Optional) save processed data for reproducibility
# dir.create("data", showWarnings = FALSE)
# readr::write_csv(data_all, file = "data/data_all.csv")

# 4. First strategy: 50-day SMA momentum -------------------------------

# Add 50-day SMA momentum signal
data_all <- add_sma_momentum(data_all, window = 50)

# Backtest the 50-day SMA strategy (multi-asset, equal-weight)
bt_ma50 <- backtest_long_only(
  data        = data_all,
  signal_col  = momentum_signal,
  strategy_name = "50-day SMA momentum"
)

# Build buy-and-hold SPY benchmark
bh_spy <- data_all %>%
  filter(symbol == "SPY") %>%
  arrange(date) %>%
  mutate(
    bh_return = daily_return,
    bh_equity = cumprod(1 + tidyr::replace_na(bh_return, 0))
  ) %>%
  select(date, bh_equity)

# Compare equity curves
equity_compare <- bt_ma50$data %>%
  select(date, equity) %>%
  left_join(bh_spy, by = "date")

# Equity curve plot: 50-day SMA vs SPY buy-and-hold
p_equity <- ggplot(equity_compare, aes(x = date)) +
  geom_line(aes(y = equity), linetype = "solid") +
  geom_line(aes(y = bh_equity), linetype = "dashed") +
  labs(
    title = "Equity Curve: 50-day SMA Strategy vs Buy-and-Hold SPY",
    subtitle = "Solid: multi-asset SMA momentum; Dashed: SPY buy-and-hold",
    x = "Date",
    y = "Portfolio value (start = 1)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(p_equity)

# Quick console summary for debugging / exploration
bt_ma50$annual_return
bt_ma50$annual_vol
bt_ma50$sharpe

# 5. Visualizations (EDA) ----------------------------------------------
# NOTE: The following plots are also included in the Quarto (.qmd)
# document, where they receive figure numbers, captions, and alt text.
# Keeping them here is useful for quick iteration during development.

# Figure 1: Asset prices over time (log scale) --------------------------
# Purpose:
# - Compare growth and volatility across SPY, NVDA, UNH, RKLB, META.
# - Log scale helps compare relative changes when prices differ in level.
# - Essentially normalizes the slope for percentage moves.

p_prices <- data_all %>%
  ggplot(aes(
    x = date,
    y = price,
    color = symbol,
    linetype = symbol
  )) +
  geom_line(linewidth = 0.6) +
  scale_y_log10() +
  labs(
    title    = "Daily Adjusted Closing Prices (Log Scale)",
    subtitle = "Equity tickers: SPY, NVDA, UNH, RKLB, META",
    x        = "Date",
    y        = "Adjusted closing price (USD, log10 scale)",
    color    = "Ticker",
    linetype = "Ticker"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "right"
  )

print(p_prices)

# Figure 2: Market volatility over time (VIX) ---------------------------
# Purpose:
# - Show how overall market volatility evolves over the sample.
# - Gives context for periods where asset returns may be more extreme.

p_vix <- macro_tidy %>%
  ggplot(aes(x = date, y = vix)) +
  geom_line(linewidth = 0.6) +
  labs(
    title    = "Market Volatility Over Time",
    subtitle = "CBOE Volatility Index (VIX)",
    x        = "Date",
    y        = "VIX level (index points)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(p_vix)

# Figure 3: Distribution of daily returns for SPY -----------------------
# Purpose:
# - Explore the typical size of daily moves in SPY.
# - Connect to volatility and risk in a simple way.

p_returns_spy <- data_all %>%
  filter(symbol == "SPY",
         !is.na(daily_return)) %>%
  ggplot(aes(x = daily_return)) +
  geom_histogram(
    bins  = 40,
    color = "white"
  ) +
  labs(
    title    = "Distribution of Daily Returns – SPY",
    subtitle = "Simple daily returns based on adjusted prices",
    x        = "Daily return (proportion)",
    y        = "Number of trading days"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(p_returns_spy)

# 6. (Optional) Save plots for later use --------------------------------
# dir.create("figures", showWarnings = FALSE)
# ggsave("figures/fig1_prices_log.png",   p_prices,      width = 9, height = 5, dpi = 300)
# ggsave("figures/fig2_vix.png",         p_vix,         width = 9, height = 4, dpi = 300)
# ggsave("figures/fig3_spy_returns.png", p_returns_spy, width = 7, height = 4, dpi = 300)
# ggsave("figures/fig4_equity.png",      p_equity,      width = 9, height = 5, dpi = 300)
