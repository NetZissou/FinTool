## --------------------------- ##
##
## Script name: Returns.R
##
## Purpose of script: Import raw data from finance terminals and calculate our asset returns
##
## Author: Net Zhang
##
## Date Created: 2020-12-31
##
## Copyright (c) Net Zhang, 2020
## Email: zhang.11091@osu.edu
##
## --------------------------- ##


# =================================================================== #
# ================== Environment Settings =========================== 
# =================================================================== #
ASSET_SYMBOLS <- c("SPY", "EFA", "IJS", "EEM", "AGG")
ASSET_WEIGHTS <- c(.25, .25, .20, .20, .10)
# Tidy toolbox ----------------------------------------------------- #
library(tidyverse)
library(lubridate)
# Fast computing --------------------------------------------------- #
library(collapse)    # fast data aggregation
library(data.table)  # Package loading and data manipulation
library(vroom)       # Package loading vroom vroom!
# Finance related -------------------------------------------------- #
library(quantmod)
library(PerformanceAnalytics)
library(tidyquant)
# Time series related ---------------------------------------------- #
library(timetk)
library(tsibble)
# library(scales)
# Visualization tools ---------------------------------------------- #
library(highcharter)


# =================================================================== #
# ================== Access Data===================================== 
# =================================================================== #

#' Get the price data.
#' 
#' @param symbols a asset symbol vector.
#' @param src the source of the data.
#' @param from start date
#' @param to end date
#' @param type Ad for Adjusted Price Cl for Closed Price
#' @return A xts object according to the assets in \code{symbols}.
get_symbols_price <- function(
  symbols = ASSET_SYMBOLS,
  src = "yahoo",
  from = "2015-12-31", to = "2020-12-31",
  type = "Ad",
  auto.assign = TRUE,
  warnings = FALSE
) {

  getSymbols(symbols,
             src = src,
             from = from, to = to,
             auto.assign = auto.assign,
             warnings = FALSE
  ) %>%
    # Adjusted Price or Closed Price
    purrr::when(
      type == "Ad" ~ map(., ~Ad(get(.x))),
      type == "Cl" ~ map(., ~Cl(get(.x)))
    ) %>%
    reduce(merge) %>%
    `colnames<-`(ASSET_SYMBOLS)
}

# =================================================================== #
# ================ Daily Price to Monthly Returns =================== 
# =================================================================== #

#' Calculate the monthly returns of our asset in an xts object.
#' 
#' @param prices a xts object.
#' @param indexAt lastof (last of the month),  firstof (first of the month).
#' @param method calculate log returns
#' @return A xts object that contains the monthly returns of our asset.
#' 
to_monthly_return_xts <- function(prices, indexAt = "lastof", method = "log") {
  # Keep the last day of each month
  asset_returns <- to.monthly(prices,
             indexAt = indexAt, 
             OHLC = FALSE
             ) %>%
    # Calculate the month log returns
    Return.calculate(., method = method) %>%
    na.omit()
  return(asset_returns)
}

to_monthly_return_tbl <- function(prices) {
  # Collapse version
  # prices %>%
  #   as_tibble(rownames = "date") %>%
  #   ftransform(year = year(date), month = month(date)) %>%
  #   fgroup_by(year, month) %>%
  #   flast() %>%
  #   fselect(-year, -month)
  
  # Tidyverse by hand calculate
  asset_returns <- prices %>%
    # Get the last day of each month
    # to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
    as_tibble(rownames = "date") %>%
    group_by(year = year(date), month = month(date)) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    select(-year, -month) %>%
    # Calculate log monthly returns
    pivot_longer(!date, values_to = "prices", names_to = "asset") %>%
    group_by(asset) %>%
    mutate(returns = (log(prices) - log(lag(prices)))) %>%
    select(-prices) %>%
    pivot_wider(names_from = asset, values_from = returns) %>%
    na.omit()
  return(asset_returns)
}

# =================================================================== #
# ================ Visualizing Asset Returns ======================== 
# =================================================================== #

# reduce(as_tibble(asset_returns_xts), test_fun, .init = h)


monthly_return_hchart <- function(returns) {
  # Get all the assets option
  assets <- names(returns)
  # Set the baseline chart
  hc <- highchart(type = "stock") %>%
    hc_title(text = "Monthly Log Returns") %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_navigator(enabled = TRUE) %>%
    hc_scrollbar(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_tooltip(valueDecimals = 2) %>%
    hc_legend(enabled = TRUE)
  for (asset in assets) {
    hc <- hc %>%
      hc_add_series(returns[, asset], name = asset)
  }
  return(hc)
}

monthly_return_distribution <- function(returns) {
  
  if (!is_tibble(returns)) {
    returns <- as_tibble(returns, rownames = "date")
  }
  returns %>%
    pivot_longer(-date, values_to = "returns", names_to = "asset") %>%
    ggplot(aes(x = returns)) +
    geom_density(aes(color = asset), alpha = 1, size = 1) +
    geom_histogram(aes(fill = asset), alpha = 0.5, binwidth = 0.01) +
    guides(color = FALSE) +
    facet_wrap(~asset) +
    labs(x = "monthly log returns", y = "distribution", fill = "Asset",
         title = "Monthly Log Returns Distribution") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# =================================================================== #
# ================ Portfolio Returns ================================ 
# =================================================================== #

portfolio_returns_xts <- function(asset_returns_xts, asset_weights, rebalance_mode = "months") {
  portfolio_returns <- 
    Return.portfolio(
      asset_returns_xts,
      weights = asset_weights,
      rebalance_on = rebalance_mode
    ) %>%
    `colnames<-`("returns")
  return(portfolio_returns)
}

portfolio_returns_tbl <- function(asset_returns_tbl, asset_weights) {
  
  # to long format
  asset_returns_tbl <- asset_returns_tbl %>%
    pivot_longer(-date, names_to = "asset", values_to = "returns")
  # Get asset - weight sheet
  asset_weight_tbl <- tibble(
    asset = asset_returns_tbl %>% distinct(asset) %>% pull(),
    weight = asset_weights
  )
  # Merge tables to get corresponding weights
  asset_returns_tbl <- asset_returns_tbl %>%
    left_join(
      asset_weight_tbl, by = "asset"
    )
  # Calculate portfolio returns
  portfolio_returns <- asset_returns_tbl %>%
    mutate(
      returns = returns * weight
    ) %>%
    select(date, returns) %>%
    group_by(date) %>%
    summarise_at(vars(returns), list(sum))
  
  return(portfolio_returns)
}



