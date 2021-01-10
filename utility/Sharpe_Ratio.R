## --------------------------- ##
##
## Script name: Sharpe_Ratio.R
##
## Purpose of script: calculate and visualize Sharpe Ratio, and the rolling Sharpe Ratio
##
## Author: Net Zhang
##
## Date Created: 2021-01-10
##
## Copyright (c) Net Zhang, 2020
## Email: zhang.11091@osu.edu
##
## --------------------------- ##

# =================================================================== #
# ================ Sharpe Ratio: Calculation ======================== 
# =================================================================== #

# Sharpe Ratio = mean(R_p - R_f) / sigma_excess
# R_p: excess return;  R_f: risk free rate;
# sigma_excess: std of the excess returns
# Therefore, higher Sharpe Ratio indicates a 'better ' portfolio

# choose a risk-free rate (RFR) ------------------------------------- #
RFR <- 0.0003 # 0.3%

get_portfolio_sharpe_ratio <- function(portfolio_returns, risk_free_rate = RFR) {
  if ("xts" %in% class(portfolio_returns)) {
    # xts Object
    portfolio_sharpe_ratio <- SharpeRatio(portfolio_returns, Rf = risk_free_rate, FUN = "StdDev") %>%
      `colnames<-`("sharpe_ratio") %>%
      as.numeric()
  } else if ("tbl" %in% class(portfolio_returns)) {
    # tbl object
    portfolio_sharpe_ratio <- 
      portfolio_returns %>%
      summarise(sharpe_ratio = mean(returns - risk_free_rate)/sd(returns - risk_free_rate)) %>%
      pull(sharpe_ratio)
  }
  return(portfolio_sharpe_ratio)
}

get_market_sharpe_ratio <- function(target = "SPY", risk_free_rate = RFR, 
                                    from = START_DATE, to = STOP_DATE) {
  market_prices <- get_symbols_price(
    symbols = target,
    src = "yahoo",
    from = from, to = to,
    type = "Ad",
    auto.assign = TRUE,
    warnings = FALSE
  )
  market_returns <- to_monthly_return_xts(market_prices, indexAt = "lastof", method = "log")
  
  market_sharpe_ratio <- 
    get_sharpe_ratio(market_returns, risk_free_rate)
  
  return(market_sharpe_ratio)
}

get_portfolio_rolling_sharpe_ratio <- function(portfolio_returns, risk_free_rate = RFR, window = 24) {
  
  sharpe_ratio_roll <- function(x) SharpeRatio(x, Rf = risk_free_rate, FUN = "StdDev")
  
  if ("xts" %in% class(portfolio_returns)) {
    # xts Object
    portfolio_rolling_sharpe_ratio <- 
      portfolio_returns %>%
      rollapply(FUN = sharpe_ratio_roll, 
                width = window) %>%
      na.omit() %>%
      `colnames<-`("rolling_sharpe_ratio")
  } else if ("tbl" %in% class(portfolio_returns)) {
    # tbl object
    portfolio_rolling_sharpe_ratio <- 
      portfolio_returns %>%
      mutate(date = ymd(date)) %>%
      tidyquant::tq_mutate(
        mutate_fun = rollapply,
        width = window,
        FUN = sharpe_ratio_roll,
        col_rename = "rolling_sharpe_ratio"
      ) %>%
      select(date, rolling_sharpe_ratio) %>%
      na.omit()
  }
  return(portfolio_rolling_sharpe_ratio)
}

plot_sharpe_ratio_comparison_hc <- function(
  portfolio_returns , risk_free_rate = RFR, 
  target = "SPY", from = START_DATE, to = STOP_DATE) {
  
  # Portfolio sharpe ratio
  portfolio_rolling_sharpe_ratio <- 
    get_portfolio_rolling_sharpe_ratio(portfolio_returns, risk_free_rate = RFR, window = 24)
  portfolio_sharpe_ratio <-
    get_portfolio_sharpe_ratio(portfolio_returns, risk_free_rate = RFR)
  
  # Market sharpe ratio
  market_prices <- get_symbols_price(
    symbols = target,
    src = "yahoo",
    from = from, to = to,
    type = "Ad",
    auto.assign = TRUE,
    warnings = FALSE
  )
  market_returns <- to_monthly_return_xts(market_prices, indexAt = "lastof", method = "log")
  
  market_rolling_sharpe_ratio <- get_portfolio_rolling_sharpe_ratio(market_returns)
  market_sharpe_ratio <- get_market_sharpe_ratio(target = "SPY", risk_free_rate = RFR, 
                                                 from = from, to = to)
  # Subtitle
  subtitle <- ifelse(portfolio_sharpe_ratio < market_sharpe_ratio,
                     paste0("Overall, your portfolio underperformed the markets with the sharpe ratio of ", 
                            round(portfolio_sharpe_ratio, 2), "%."),
                     paste0("Congrats! Overall, your portfolio outperformed the markets with the sharpe ratio of ",
                            round(portfolio_sharpe_ratio, 2), "%.")        
                     )
  
  # Set the baseline chart
  hc <- highchart(type = "stock") %>%
    hc_title(text = "24-Month Rolling Sharpe Ratio") %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_navigator(enabled = TRUE) %>%
    hc_scrollbar(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
  # Add rolling series
  hc <- hc %>%
    hc_add_series(round(portfolio_rolling_sharpe_ratio, 4), name = "Portfolio") %>%
    hc_add_series(round(market_rolling_sharpe_ratio, 4), name = target) %>%
    hc_yAxis(
      plotLines = list(
        list(
          label = list(text = paste0("Risk Free Rate: ", round(RFR*100, 2), "%")),
          color = "#FF0000",
          width = 2,
          value = RFR,
          # the zIndex is used to put the label text over the grid lines 
          zIndex = 1
        )
      ),
      labels = list(format = "{value}%")
    ) %>%
    hc_subtitle(
      text = subtitle
    )
  return(hc)
}



























