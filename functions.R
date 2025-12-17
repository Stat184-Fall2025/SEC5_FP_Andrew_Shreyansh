# functions.R ------------------------------------------------------------
# Utility functions for data ingest and tidying

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

#' (Placeholder) Compute daily returns
#'
#' @param data_all Tibble with symbol, date, price.
#'
#' @return Tibble with an added daily_return column.``
#' @details
#' This is where we will continue later for the momentum / backtesting part
#' of the project.
add_daily_returns <- function(data_all) {
  data_all %>%
    group_by(symbol) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(daily_return = price / lag(price) - 1) %>%
    ungroup()
}
