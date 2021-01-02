## --------------------------- ##
##
## Script name: risks.R
##
## Purpose of script: Calculate std, skewness, and kurtosis statistics 
##                    to explore the variability of portfolio returns
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
# ================== Standard Deviation ============================= 
# =================================================================== #
get_portfolio_mean <- function(portfolio_returns) {
  if ("xts" %in% class(portfolio_returns)) {
    # xts Object
    portfolio_mean <- as.numeric(mean(portfolio_returns))
  } else if ("tbl" %in% class(portfolio_returns)) {
    # tbl object
    portfolio_mean <- portfolio_returns %>%
      summarise(mean = mean(returns)) %>%
      pull(mean)
  }
  return(portfolio_mean)
}

get_portfolio_sd <- function(portfolio_returns) {
  if ("xts" %in% class(portfolio_returns)) {
    # xts Object
    portfolio_sd <- as.numeric(StdDev(portfolio_returns)[1,1])
  } else if ("tbl" %in% class(portfolio_returns)) {
    # tbl object
    portfolio_sd <- portfolio_returns %>%
      summarise(sd = sd(returns)) %>%
      pull(sd)
  }
  return(portfolio_sd)
}

plot_sd_overtime <- function(portfolio_returns) {
  if ("xts" %in% class(portfolio_returns)) {
    portfolio_returns <- as_tibble(rownames = "date")
  }
  # Get portfolio mean & sd
  portfolio_sd <- get_portfolio_sd(portfolio_returns)
  portfolio_mean <- get_portfolio_mean(portfolio_returns)
  
  # Color the observations
  levels <- c("Overwhelming", "Expected", "Warning")
  portfolio_returns %>%
    mutate(
      date = ymd(date),
      status = case_when(
        returns <= (portfolio_mean - portfolio_sd) ~ factor("Warning", levels),
        returns >= (portfolio_mean + portfolio_sd) ~ factor("Overwhelming", levels),
        TRUE ~ factor("Expected", levels)
      )
    ) %>%
    ggplot(aes(x = date, y = returns)) +
    geom_point(aes(color = status, size = returns), 
               shape = 19, alpha = 0.7) +
    geom_hline(yintercept = (portfolio_mean + portfolio_sd), 
               color = "purple", linetype = "dotted", size = 1.2) +
    geom_hline(yintercept = (portfolio_mean - portfolio_sd), 
               color = "purple", linetype = "dotted", size = 1.2) +
    labs(x = "date", y = "monthly log returns", title = "Portfolio Performance - SD") +
    theme_minimal() +
    guides(size = FALSE) +
    # Percentage
    scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
    # Date Breaks
    scale_x_date(date_breaks = "6 month") +
    # Center title
    theme(plot.title = element_text(hjust = 0.5))
}

plot_sd_comparison <- function(asset_returns, asset_weights) {
  if ("xts" %in% class(asset_returns)) {
    asset_returns <- as_tibble(rownames = "date")
  }
  portfolio_returns <- to_portfolio_returns_tbl(asset_returns, asset_weights)
  # Portfolio Sd
  portfolio_returns_sd <- get_portfolio_sd(portfolio_returns)
  # Asset Sd + Portfolio Sd
  asset_returns %>%
    summarise_at(vars(-date), .funs = list(sd)) %>%
    add_column(
      Portfolio = portfolio_returns_sd
    ) %>%
    # pivot every columns into a long format
    pivot_longer(everything(), names_to = "asset", values_to = "sd") %>%
    ggplot(aes(x = asset, y = sd)) +
    geom_col(aes(fill = asset)) +
    labs(x = "Asset", y = "Standard Deviation", title = "Portfolio Performance - SD Comparision") +
    theme_minimal() +
    # Center title
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2))
}

plot_return_vs_risk <- function(asset_returns, asset_weights) {
  
  if ("xts" %in% class(asset_returns)) {
    asset_returns <- as_tibble(rownames = "date")
  }
  portfolio_returns <- to_portfolio_returns_tbl(asset_returns, asset_weights)
  # Portfolio sd & mean
  portfolio_returns_sd <- get_portfolio_sd(portfolio_returns)
  portfolio_returns_mean <- get_portfolio_mean(portfolio_returns)
  # Asset sd & mean
  asset_returns %>%
    summarise_at(vars(-date), .funs = list(sd)) %>%
    add_column(
      Portfolio = portfolio_returns_sd
    ) %>%
    pivot_longer(everything(), names_to = "asset", values_to = "sd") %>%
    left_join(
      asset_returns %>%
        summarise_at(vars(-date), .funs = list(mean)) %>%
        add_column(
          Portfolio = portfolio_returns_mean
        ) %>%
        pivot_longer(everything(), names_to = "asset", values_to = "mean"),
      by = "asset"
    ) %>%
    ggplot(aes(x = sd, y = mean)) +
    geom_point(aes(color = asset), size = 5, 
               shape = 19, alpha = 0.7) +
    labs(x = "Standard Deivation", y = "Expected Returns", 
         title = "Portfolio Performance - Expected Monthly Return versus Risk") +
    theme_minimal() +
    # Center title
    theme(plot.title = element_text(hjust = 0.5)) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 2))
}

get_portfolio_rolling_mean <- function(portfolio_returns, window = 24) {
    if ("xts" %in% class(portfolio_returns)) {
      portfolio_rolling_mean <- 
        portfolio_returns %>%
        rollapply(FUN = mean, width = window) %>%
        na.omit() %>%
        `colnames<-`("rolling_mean")
    } else if ("tbl" %in% class(portfolio_returns)) {
      portfolio_rolling_mean <- 
        portfolio_returns_tbl %>%
        mutate(
          rolling_mean = RcppRoll::roll_mean(returns, n = window, align="right", fill=NA)
        ) %>%
        select(date, rolling_mean) %>%
        na.omit()
    }
  return(portfolio_rolling_mean)
}

get_portfolio_rolling_sd <- function(portfolio_returns, window = 24) {
  if ("xts" %in% class(portfolio_returns)) {
    portfolio_rolling_sd <- 
      portfolio_returns %>%
      rollapply(FUN = sd, width = window) %>%
      na.omit() %>%
      `colnames<-`("rolling_sd")
  } else if ("tbl" %in% class(portfolio_returns)) {
    portfolio_rolling_sd <- 
      portfolio_returns_tbl %>%
      mutate(
        rolling_sd = RcppRoll::roll_sd(returns, n = window, align="right", fill=NA)
      ) %>%
      select(date, rolling_sd) %>%
      na.omit()
  }
  return(portfolio_rolling_sd)
}

plot_rolling_volatility <- function(portfolio_rolling_sd) {
  # Set the baseline chart
  hc <- highchart(type = "stock") %>%
    hc_title(text = "24-Month Rolling Volatility") %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_navigator(enabled = TRUE) %>%
    hc_scrollbar(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = TRUE) %>%
    hc_yAxis(labels = list(format = "{value}%"), opposite = FALSE)
  # Add rolling series
  hc <- hc %>%
    hc_add_series(round(portfolio_rolling_sd, 4) * 100)
  return(hc)
}

# =================================================================== #
# ================== Skewness ======================================= 
# =================================================================== #













