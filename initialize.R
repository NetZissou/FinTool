# =================================================================== #
# ================== Environment Settings =========================== 
# =================================================================== #
Sys.setenv(LANG = "en")
# ASSET_SYMBOLS <- c("SPY", "EFA", "IJS", "EEM", "AGG")
# ASSET_WEIGHTS <- c(.25, .25, .20, .20, .10)
DEFAULT_START_DATE <- "2012-12-31"
DEFAULT_STOP_DATE <- "2017-12-31"
# Tidy toolbox ----------------------------------------------------- #
library(tidyverse)
library(lubridate)
library(forcats)
library(DT)
library(formattable)
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
# ================== Data Fetch Function ============================ 
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
  symbols,
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


