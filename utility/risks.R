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
# Portfolio Statistics ---------------------------------------------- #
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

get_portfolio_median <- function(portfolio_returns) {
  if ("xts" %in% class(portfolio_returns)) {
    # xts Object
    portfolio_median <- as.numeric(median(portfolio_returns))
  } else if ("tbl" %in% class(portfolio_returns)) {
    # tbl object
    portfolio_median <- portfolio_returns %>%
      summarise(median = median(returns)) %>%
      pull(median)
  }
  return(portfolio_median)
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

# Portfolio rolling statistics -------------------------------------- #
get_portfolio_rolling_mean <- function(portfolio_returns, window = 24) {
  if ("xts" %in% class(portfolio_returns)) {
    portfolio_rolling_mean <- 
      portfolio_returns %>%
      rollapply(FUN = mean, width = window) %>%
      na.omit() %>%
      `colnames<-`("rolling_mean")
  } else if ("tbl" %in% class(portfolio_returns)) {
    portfolio_rolling_mean <- 
      portfolio_returns %>%
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
      portfolio_returns %>%
      mutate(
        rolling_sd = RcppRoll::roll_sd(returns, n = window, align="right", fill=NA)
      ) %>%
      select(date, rolling_sd) %>%
      na.omit()
  }
  return(portfolio_rolling_sd)
}

# Visualizations on Volatility -------------------------------------- #

# 1) plot sd overtime
plot_sd_overtime <- function(portfolio_returns) {
  if ("xts" %in% class(portfolio_returns)) {
    portfolio_returns <- as_tibble(portfolio_returns, rownames = "date")
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

plot_sd_overtime_hc <- function(portfolio_returns) {
  if ("xts" %in% class(portfolio_returns)) {
    portfolio_returns <- as_tibble(portfolio_returns, rownames = "date")
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
    hchart("line", hcaes(x = date, y = returns, color = status)) %>%
    hc_tooltip(valueDecimals = 2) %>%
    hc_legend(enabled = TRUE) %>%
    hc_navigator(enabled = TRUE) %>%
    hc_scrollbar(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_xAxis(
      title = FALSE,
      type = "datetime"
    ) %>%
    hc_yAxis(
      title = list(text = "Monthly Log Returns"),
      plotBands = list(
        list(
          from = portfolio_mean - portfolio_sd,
          to = portfolio_mean + portfolio_sd,
          color = hex_to_rgba("green", 0.1),
          label = list(text = "Within one Std Region"),
          # the zIndex is used to put the label text over the grid lines 
          zIndex = 1
        )
      )
    ) %>%
    hc_title(
      text = "Portfolio Performance - SD",
      align = "center"
    ) %>%
    hc_add_theme(hc_theme_flat())
}

# 2) plot sd comparison
plot_sd_comparison <- function(asset_returns, asset_weights) {
  if ("xts" %in% class(asset_returns)) {
    asset_returns <- as_tibble(asset_returns, rownames = "date")
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

plot_sd_comparison_hc <- function(asset_returns, asset_weights) {
  if ("xts" %in% class(asset_returns)) {
    asset_returns <- as_tibble(asset_returns, rownames = "date")
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
    hchart("column", hcaes(x = asset, y = sd, group = asset)) %>%
    hc_tooltip(valueDecimals = 4) %>%
    hc_xAxis(
      title = list(text = "Asset")
    ) %>%
    hc_yAxis(
      title = list(text = "Standard Deviation")
    ) %>%
    hc_title(
      text = "Portfolio Performance - SD Comparision",
      align = "center"
    ) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE)
}

# 3) plot return vs risks
plot_return_vs_risk <- function(asset_returns, asset_weights) {
  
  if ("xts" %in% class(asset_returns)) {
    asset_returns <- as_tibble(asset_returns, rownames = "date")
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

plot_return_vs_risk_hc <- function(asset_returns, asset_weights) {
  
  if ("xts" %in% class(asset_returns)) {
    asset_returns <- as_tibble(asset_returns, rownames = "date")
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
    hchart("scatter", hcaes(x = round(sd*100, 2), y = mean, group = asset, size  = log(mean/sd))) %>%
    hc_tooltip(valueDecimals = 4) %>%
    hc_xAxis(
      title = list(text = "Standard Deivation"),
      labels = list(format = "{value}%"), opposite = FALSE
    ) %>%
    hc_yAxis(
      title = list(text = "Expected Returns")
    ) %>%
    hc_title(
      text = "Portfolio Performance - Expected Monthly Return versus Risk",
      align = "center"
    ) %>%
    hc_subtitle(
      text = "Bubble size is calculated based on the ratio between the expected value and standard deviation.</br>
      Overall, the asset with higher expected returns and lower volatility is preferred.",
      align = "center",
      useHTML = TRUE
    ) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE)
}

# 4) plot rolling volatility
plot_rolling_volatility_hc <- function(portfolio_rolling_sd) {
  # Set the baseline chart
  hc <- highchart(type = "stock") %>%
    hc_title(text = "24-Month Rolling Volatility") %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_navigator(enabled = TRUE) %>%
    hc_scrollbar(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE) %>%
    hc_yAxis(labels = list(format = "{value}%"), opposite = FALSE)
  # Add rolling series
  hc <- hc %>%
    hc_add_series(round(portfolio_rolling_sd, 4) * 100)
  return(hc)
}

# =================================================================== #
# ================== Skewness ======================================= 
# =================================================================== #
# Portfolio Statistics ---------------------------------------------- #
get_portfolio_skew <- function(portfolio_returns) {
  
  if ("xts" %in% class(portfolio_returns)) {
    # xts Object
    portfolio_skew <- skewness(portfolio_returns$returns)
  } else if ("tbl" %in% class(portfolio_returns)) {
    # tbl object
    portfolio_skew <-
      portfolio_returns %>%
      summarise_at(vars(returns), .funs = list(skewness)) %>%
      pull()
  }
  return(portfolio_skew)
}

# Portfolio rolling statistics -------------------------------------- #
get_portfolio_rolling_skew <- function(portfolio_returns, window = 24) {
  if ("xts" %in% class(portfolio_returns)) {
    portfolio_rolling_skew <- 
      portfolio_returns %>%
      rollapply(FUN = skewness, width = window) %>%
      na.omit() %>%
      `colnames<-`("rolling_skew")
  } else if ("tbl" %in% class(portfolio_returns)) {
    portfolio_rolling_skew <- 
      portfolio_returns %>%
      mutate(date = ymd(date)) %>%
      tidyquant::tq_mutate(
        mutate_fun = rollapply,
        width = window,
        FUN = skewness,
        col_rename = "rolling_skew"
      ) %>%
      select(date, rolling_skew) %>%
      na.omit()
  }
  return(portfolio_rolling_skew)
}

# Visualizations on Volatility -------------------------------------- #

# 1) plot density to illustrate skewness
plot_skew_density_hc <- function(portfolio_returns) {
  if ("xts" %in% class(portfolio_returns)) {
    # xts Object
    portfolio_returns <- as_tibble(portfolio_returns, rownames = "date")
  }
  
  density <- portfolio_returns %>% pull(returns) %>% density()
  portfolio_mean <- get_portfolio_mean(portfolio_returns)
  portfolio_median <- get_portfolio_median(portfolio_returns)
  skewness_type <- ifelse(portfolio_mean < portfolio_median, 
                          "negative(left) skewed", "positive(right) skewed")
  skewness_stat <- get_portfolio_skew(portfolio_returns)
  
  hchart(density, type = "area", name = "returns") %>%
    hc_title(
      text = "Portfolio Performance - Density Plot Illustrating Skewness",
      align = "center"
    ) %>%
    hc_subtitle(
      text = paste0("The portfolio returns are ", skewness_type, 
                    " overtime, with the skewness of: ", 
                    round(skewness_stat, 4)
                    ),
      align = "center"
    ) %>%
    hc_xAxis(
      plotLines = list(
        # Mean
        list(
          label = list(text = "Mean"),
          color = "black",
          width = 2,
          value = portfolio_mean
        ),
        # Median
        list(
          label = list(text = "Median"),
          color = "#FF0000",
          width = 2,
          value = portfolio_median
        )
      )
    ) %>%
    hc_tooltip(valueDecimals = 4) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE)
}
# 2) plot skewness comparison

plot_skew_comparison_hc <- function(asset_returns, asset_weights)  {
  if ("xts" %in% class(asset_returns)) {
    asset_returns <- as_tibble(asset_returns, rownames = "date")
  }
  portfolio_returns <- to_portfolio_returns_tbl(asset_returns, asset_weights)
  
  # Portfolio skewness
  portfolio_returns_skew <- get_portfolio_skew(portfolio_returns)
  # Asset Sd + Portfolio Sd
  asset_returns %>%
    summarise_at(vars(-date), .funs = list(skewness)) %>%
    add_column(
      Portfolio = portfolio_returns_skew
    ) %>%
    # pivot every columns into a long format
    pivot_longer(everything(), names_to = "asset", values_to = "skewness") %>%
    hchart("column", hcaes(x = asset, y = skewness, group = asset)) %>%
    hc_tooltip(valueDecimals = 4) %>%
    hc_xAxis(
      title = list(text = "Asset")
    ) %>%
    hc_yAxis(
      title = list(text = "Skewness")
    ) %>%
    hc_title(
      text = "Portfolio Performance - Skewness Comparision",
      align = "center"
    ) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE)
}
# 3) plot rolling skewness
plot_rolling_skew_hc <- function(portfolio_rolling_skew) {
  # Set the baseline chart
  hc <- highchart(type = "stock") %>%
    hc_title(text = "24-Month Rolling Skewness") %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_navigator(enabled = TRUE) %>%
    hc_scrollbar(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
  # Add rolling series
  hc <- hc %>%
    hc_add_series(round(portfolio_rolling_skew, 4))
  return(hc)
}























