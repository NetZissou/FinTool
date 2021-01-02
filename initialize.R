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

