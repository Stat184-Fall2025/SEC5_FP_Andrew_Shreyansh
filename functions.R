# functions.R ------------------------------------------------------------
# Utility functions for data ingest, tidying, and simple strategy backtesting

library(tidyverse)
library(tidyquant)

#' Get asset price data from Yahoo Finance
#'
#' @param symbols Character vector of tickers (e.g. c("SPY", "NVDA")).
#' @param start Start date (Date).
#' @param end End date (Date).
#'
#' @return A tibble with raw price data for all tickers.
get_price_data <- function(symbols, start, end) {
  tq_get(
    x    = symbols,
    get  = "stock.prices",
    from = start,
    to   = end
  )
}

#' Get macro / volatility data
#'
#' Currently pulls:
#' - ^VIX (implied volatility index) from Yahoo
#' - TB3MS (3-month T-bill) from FRED
#'
#' @param start Start date (Date).
#' @param end End date (Date).
#'
#' @return A tibble with columns: date, vix, tbill_3m.
get_macro_data <- function(start, end) {
  # VIX from Yahoo (^VIX)
  vix_raw <- tq_get(
    x    = "^VIX",
    get  = "stock.prices",
    from = start,
    to   = end
  )
  
  # 3-month T-bill rate from FRED (TB3MS)
  tbill_raw <- tq_get(
    x    = "TB3MS",
    get  = "economic.data",
    from = start,
    to   = end
  )
  
  vix_tidy <- vix_raw %>%
    select(date, vix = adjusted)
  
  tbill_tidy <- tbill_raw %>%
    select(date, tbill_3m = price)
  
  vix_tidy %>%
    full_join(tbill_tidy, by = "date") %>%
    arrange(date)
}

#' Tidy asset price data
#'
#' @param prices_raw Tibble returned by get_price_data().
#'
#' @return Tibble with columns: symbol, date, price.
tidy_price_data <- function(prices_raw) {
  prices_raw %>%
    select(symbol, date, price = adjusted) %>%
    arrange(symbol, date)
}

#' Tidy macro data
#'
#' @param macro_raw Tibble returned by get_macro_data().
#'
#' @return Tibble with columns: date, vix, tbill_3m (ordered by date).
tidy_macro_data <- function(macro_raw) {
  macro_raw %>%
    arrange(date)
}

#' Combine price and macro data
#'
#' Left-joins macro variables (vix, tbill_3m) onto each symbol/date.
#'
#' @param prices_tidy Tidy asset prices (symbol, date, price).
#' @param macro_tidy Tidy macro data (date, vix, tbill_3m).
#'
#' @return Tibble with symbol, date, price, vix, tbill_3m.
combine_price_macro <- function(prices_tidy, macro_tidy) {
  prices_tidy %>%
    left_join(macro_tidy, by = "date")
}

#' Compute simple daily returns
#'
#' @param data_all Tibble with at least symbol, date, and price.
#'
#' @return Tibble with an added daily_return column.
#' @details
#' Daily returns are calculated within each symbol as
#' (price / lag(price) - 1). The first observation for each symbol
#' will have NA for daily_return because there is no previous day.
add_daily_returns <- function(data_all) {
  data_all %>%
    group_by(symbol) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(daily_return = price / dplyr::lag(price) - 1) %>%
    ungroup()
}

#' Add simple moving average (SMA) and momentum signal
#'
#' @param data_all Tibble with columns symbol, date, price (and possibly others).
#' @param window Integer, number of days for the moving average.
#'
#' @return Tibble with added columns: sma, momentum_signal (0/1).
#' @details
#' Within each symbol, we compute a simple moving average over the chosen
#' window and define a binary signal:
#' - 1 when price > SMA (uptrend / "in the market")
#' - 0 otherwise (out of the market)
add_sma_momentum <- function(data_all, window = 50) {
  data_all %>%
    group_by(symbol) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      sma = TTR::SMA(price, n = window),
      momentum_signal = dplyr::if_else(price > sma, 1, 0, missing = 0)
    ) %>%
    ungroup()
}

#' Backtest a simple long-only strategy
#'
#' @param data Tibble with columns: date, symbol, daily_return, and a signal column.
#' @param signal_col Name of the column containing 0/1 trading signal.
#' @param strategy_name Label for the strategy.
#'
#' @return A list containing equity curve data and summary metrics.
#' @details
#' The backtest:
#' - Uses the *lagged* signal (yesterday's signal for today's return)
#'   to avoid look-ahead bias.
#' - Computes per-symbol strategy returns as signal * daily_return.
#' - Averages across symbols each day (equal-weight across active assets).
#' - Builds an equity curve from cumulative product of (1 + daily return).
backtest_long_only <- function(data, signal_col, strategy_name = "strategy") {
  signal_col <- rlang::ensym(signal_col)
  
  df <- data %>%
    arrange(date, symbol) %>%
    group_by(symbol) %>%
    mutate(
      signal_lag   = dplyr::lag(!!signal_col, default = 0),
      strat_return = signal_lag * daily_return
    ) %>%
    ungroup() %>%
    group_by(date) %>%
    summarise(
      strategy_return = if (all(is.na(strat_return))) NA_real_ else mean(strat_return, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      equity = cumprod(1 + tidyr::replace_na(strategy_return, 0))
    )
  
  total_return <- dplyr::last(df$equity, default = 1) - 1
  n_days       <- nrow(df)
  annual_return <- if (n_days > 0) (1 + total_return)^(252 / n_days) - 1 else NA_real_
  annual_vol    <- stats::sd(df$strategy_return, na.rm = TRUE) * sqrt(252)
  sharpe        <- ifelse(annual_vol > 0, annual_return / annual_vol, NA_real_)
  
  list(
    name          = strategy_name,
    data          = df,
    total_return  = total_return,
    annual_return = annual_return,
    annual_vol    = annual_vol,
    sharpe        = sharpe
  )
}
