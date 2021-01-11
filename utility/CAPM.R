## --------------------------- ##
##
## Script name: CAPM.R
##
## Purpose of script: calculate and visualize CAPM parameters
##
## Author: Net Zhang
##
## Date Created: 2021-01-10
##
## Copyright (c) Net Zhang, 2020
## Email: zhang.11091@osu.edu
##
## --------------------------- ##

# Well, Capital Asset Pricing Model, aka CAPM, basically a linear regression model,
# introduced back in 1964 and garnered a Nobel for its creator - William Sharpe.

get_CAPM_beta <- function(portfolio_returns, market_returns){
  if ("xts" %in% class(portfolio_returns)) {
    # xts Object
    CAPM_beta <- 
      CAPM.beta(portfolio_returns, market_returns)
  } else if ("tbl" %in% class(portfolio_returns)) {
    # tbl object
    market_returns <- xts_to_tbl(market_returns)
    CAPM_beta <- portfolio_returns %>%
      bind_cols(
        xts_to_tbl(market_returns) %>% 
          select(-date) %>% set_names("target")
      ) %>%
      lm(returns ~ target, data = .) %>%
      broom::tidy() %>%
      filter(term == "target") %>%
      pull(estimate)
  }
  return(CAPM_beta)
}


plot_CAPM_hc <- function(portfolio_returns, market_returns) {
  # Merge plot data
  portfolio_returns <- xts_to_tbl(portfolio_returns)
  target_market_name <- names(market_returns)
  market_returns <- xts_to_tbl(market_returns)
  portfolio_model_augmented <- portfolio_returns %>%
    bind_cols(
      xts_to_tbl(market_returns) %>% 
        select(-date) %>% set_names("mkt_returns")
    ) %>%
    lm(returns ~ mkt_returns, data = .) %>%
    broom::augment() %>%
    bind_cols(
      portfolio_returns %>% select(date)
    )
  
  hc <- highchart() %>%
    hc_title(text = "Scatter with Regression Fit") %>%
    hc_add_series(portfolio_model_augmented, type = "scatter",
                  hcaes(x = round(mkt_returns, 4), y = round(returns, 4), date = date),
                  name = "Returns") %>%
    hc_add_series(portfolio_model_augmented, type = "line",
                  hcaes(x = mkt_returns, y = .fitted),
                  name = "CAPM Beta") %>%
    hc_xAxis(title = list(text = "Market Returns")) %>%
    hc_yAxis(title = list(text = "Portforlio Returns")) %>%
    hc_tooltip(formatter = JS("function(){
                              return (
                              'port return: ' + this.y.toFixed(4) + 
                              ' <br> mkt return: ' + this.x.toFixed(4) +
                              ' <br> date: ' + this.point.date
                              )
    }")) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_legend(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE)

  return(hc)
}

