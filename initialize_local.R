## --------------------------- ##
##
## Script name: initialize.R
##
## Purpose of script: Import raw data from finance terminals
##
## Author: Net Zhang
##
## Date Created: 2021-01-02
##
## Copyright (c) Net Zhang, 2021
## Email: zhang.11091@osu.edu
##
## --------------------------- ##


# =================================================================== #
# ================== Environment Settings =========================== 
# =================================================================== #
Sys.setenv(LANG = "en")
ASSET_SYMBOLS <- c("SPY", "EFA", "IJS", "EEM", "AGG")
ASSET_WEIGHTS <- c(.25, .25, .20, .20, .10)
START_DATE <- "2012-12-31"
STOP_DATE <- "2017-12-31"
# Tidy toolbox ----------------------------------------------------- #
library(tidyverse)
library(lubridate)
library(forcats)
# Fast computing --------------------------------------------------- #
#library(collapse)    # fast data aggregation
#library(data.table)  # Package loading and data manipulation
#library(vroom)       # Package loading vroom vroom!
# Finance related -------------------------------------------------- #
library(quantmod)
library(PerformanceAnalytics)
#library(tidyquant)
# Time series related ---------------------------------------------- #
# library(timetk)
# library(tsibble)
# library(scales)
# Visualization tools ---------------------------------------------- #
library(highcharter)
# Self-developed Utility Functions --------------------------------- #
source("utility/returns.R")
source("utility/risks.R")
source("utility/sharpe_ratio.R")
source("utility/CAPM.R")
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
             warnings = warnings
  ) %>%
    # Adjusted Price or Closed Price
    purrr::when(
      type == "Ad" ~ map(., ~Ad(get(.x))),
      type == "Cl" ~ map(., ~Cl(get(.x)))
    ) %>%
    reduce(merge) %>%
    `colnames<-`(symbols)
}

xts_to_tbl <- function(object) {
  
  if("xts" %in% class(object)) {
    object<- object %>%
      as_tibble(
        rownames = "date"
      ) %>%
      mutate(date= ymd(date))
  }
  return(object)
}

prices <- get_symbols_price(
  symbols = ASSET_SYMBOLS,
  src = "yahoo",
  from = START_DATE, to = STOP_DATE,
  type = "Ad",
  auto.assign = TRUE,
  warnings = FALSE
)

# =================================================================== #
# ================== Returns ======================================== 
# =================================================================== #

# Monthly log returns -- Assets
asset_returns_xts <- 
  to_monthly_return_xts(prices, indexAt = "lastof", method = "log")
asset_returns_tbl <- 
  to_monthly_return_tbl(prices)

## Monthly log returns -- portfolio

portfolio_returns_xts <- 
  to_portfolio_returns_xts(asset_returns_xts, ASSET_WEIGHTS, rebalance_mode = "months")
portfolio_returns_tbl <- 
  to_portfolio_returns_tbl(asset_returns_tbl, ASSET_WEIGHTS)
market_returns_xts <- 
  get_market_returns(target = "SPY",  
                     from = START_DATE, to = STOP_DATE)
  
# =================================================================== #
# ================== Risks ========================================== 
# =================================================================== #

# Standard Deviation
portfolio_sd <- get_portfolio_sd(portfolio_returns_tbl)
portfolio_mean <- get_portfolio_mean(portfolio_returns_tbl)

portfolio_rolling_sd <- get_portfolio_rolling_sd(portfolio_returns_xts, window = 24)

plot_sd_overtime_hc(portfolio_returns_tbl)
plot_sd_comparison_hc(asset_returns_tbl, ASSET_WEIGHTS)
plot_return_vs_risk_hc(asset_returns_tbl, ASSET_WEIGHTS)
# Skewness
portfolio_skew <- get_portfolio_skew(portfolio_returns_tbl)
portfolio_rolling_skew <- get_portfolio_rolling_skew(portfolio_returns_xts, window = 24)

plot_skew_density_hc(portfolio_returns_tbl)
plot_skew_comparison_hc(asset_returns_tbl, ASSET_WEIGHTS)
plot_rolling_skew_hc(portfolio_rolling_skew)
# Kurtosis
portfolio_kurtosis <- get_portfolio_kurtosis(portfolio_returns_tbl)
portfolio_rolling_kurtosis <- get_portfolio_rolling_kurtosis(portfolio_returns_xts, window = 24)

# =================================================================== #
# ================== Sharpe Ratio ===================================
# =================================================================== #

# choose a risk-free rate (RFR) ------------------------------------- #
RFR <- 0.0003 # 0.3%

portfolio_sharpe_ratio <- 
  get_portfolio_sharpe_ratio(portfolio_returns_xts, risk_free_rate = RFR)

market_sharpe_ratio <- 
  get_market_sharpe_ratio(target = "SPY", risk_free_rate = RFR, 
                                               from = START_DATE, to = STOP_DATE)
portfolio_rolling_sharpe_ratio <- 
  get_portfolio_rolling_sharpe_ratio(portfolio_returns_xts, risk_free_rate = RFR, window = 24)

plot_sharpe_ratio_comparison_hc(portfolio_returns_xts)

# =================================================================== #
# ================== CAPM ===========================================
# =================================================================== #

CAPM_beta <- get_CAPM_beta(portfolio_returns_xts, market_returns_xts)
plot_CAPM_hc(portfolio_returns_tbl, market_returns_xts)







