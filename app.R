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
library(shiny)
library(shinydashboard)
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
# ======================================================================= #
# ========================= UI ==========================================
# ======================================================================= #
variablesUI <- function(id) {
  ns <- NS(id)
  
  tags$div(
    id=paste0("var", id),
    fluidRow(
      column(
        width = 6,
        uiOutput(ns('variable'))
      ),
      column(
        width = 6,
        uiOutput(ns('value'))
      )
    )
  )
}

portfolio_table <- function(box_width = 12, collapsible = F) {
  box(width = box_width, collapsible = collapsible,
      # Portfolio Table
      formattableOutput("outDT")
  )
}

asset_choice <- function(box_width = 12, collapsible = F) {
  box(solidHeader = T, title = "Modify Portfolio",
    width = box_width, collapsible = collapsible,
      
      # Date Selection
      fluidRow(
        column(width = 6, 
               dateInput("start_date", "Start Date", value = DEFAULT_START_DATE)
               ),
        column(width = 6,
               dateInput("stop_date", "Stop Date", value = DEFAULT_STOP_DATE)
        )
      ),
      # MODULE UI FOR VARIABLE 1
      variablesUI(1),
      
      # MODULE UI FOR VARIABLE 2
      variablesUI(2),
      
      # Where to insert the other modules
      tags$div(id = 'placeholder'),
      
      # ADD / REMOVE BUTTONS
      fluidRow(
        column(width = 4, # ADD VARIABLE BUTTON
               actionButton(inputId = 'insertVarBtn', label = "Add", width = "100%")),
        column(width = 4, # REMOVE VARIABLE BUTTON
               actionButton(inputId = 'removeVarBtn', label = "Remove", width = "100%")),
        column(width = 4, # Fetch Data
               actionButton(inputId = 'fetchData', label = "Fetch/Update", width = "100%"))
      )
  )
}

ui <- dashboardPage(
  dashboardHeader(titleWidth = 350),
  dashboardSidebar(
    disable = T
  ),
  dashboardBody(
    fluidRow(
      column(width = 4,
             portfolio_table(),
             asset_choice(collapsible = T)
             ),
      column(width = 8,
             tabBox(
               title = "Returns",
               # The id lets us use input$tabset1 on the server to find the current tab
               id = "tabset1", height = "100%", width = "100%",
               tabPanel("Asset Returns", highchartOutput("asset_monthly_return")),
               tabPanel("Portfolio Returns", "Tab content 2")
              )
             )
    )
  )
)

# ======================================================================= #
# ========================= SERVER ======================================
# ======================================================================= #
variablesServer <- function(input, output, session){
  ns = session$ns
  
  output$variable <- renderUI({
    textInput(
      inputId = ns("variable"),
      label = paste0("Asset Choice #", strsplit(x = ns(""), split = "-")),
      value = NULL,
      width = "100%"
    )
  })
  
  output$value <- renderUI({
    numericInput(
      inputId = ns('value'),
      label = paste0("Portf. % #", strsplit(x = ns(""), split = "-")),
      value = 5,
      width = "100%"
    )
  })
}

server <- function(input, output, session) {
  
  # ========================================== #
  # =========== Portfolio Selection ==========
  # ========================================== #
  # CREATE EMPTY DATAFRAME
  add.variable <- reactiveValues()
  
  add.variable$df <- data.frame(
    "variable" = character(0),
    "value" = numeric(0),
    stringsAsFactors = FALSE
  )
  
  #-------------------------- #
  # CALL MODULE 1 
  
  callModule(variablesServer, 1)
  
  ## SAVE INPUTS FROM 1 INTO DATAFRAME
  observeEvent(input[[NS(1, "variable")]], {
    add.variable$df[1, 1] <- input[[NS(1, "variable")]]
  })
  
  observeEvent(input[[NS(1, "value")]], {
    add.variable$df[1, 2] <- input[[NS(1, "value")]] / 100
  })
  
  #-------------------------- #
  # CALL MODULE 2
  callModule(variablesServer, 2)
  
  ## SAVE INPUTS FROM 2 INTO DATAFRAME
  
  observeEvent(input[[NS(2, "variable")]], {
    add.variable$df[2, 1] <- input[[NS(2, "variable")]]
  })
  
  observeEvent(input[[NS(2, "value")]], {
    add.variable$df[2, 2] <- input[[NS(2, "value")]] / 100
  })
  
  #-------------------------- #
  # START BUTTON VALUE AT 2 TO ACCOUNT FOR THE FIRST 2 VALUES
  
  btn <- reactiveValues(value = 2)
  
  #-------------------------- #
  # ADD VARIABLES
  
  observeEvent(input$insertVarBtn, {
    
    # EACH TIME THE USER CLICKS, ADD 1 TO BUTTON VALUE
    btn$value <- btn$value + 1
    
    ## WHEN WE USE btn$value DIRECTLY WE LOSE REACTIVITY
    ## PASSING IT TO btn.temp AND USING btn.tmp WORKS (SOMEHOW)
    btn.tmp <- btn$value
    
    # CALL MODULE NUMBER params$btn
    callModule(variablesServer, btn.tmp)
    
    # INSERT MODULE UI
    insertUI(
      selector = '#placeholder',
      where = "beforeEnd",
      ui = variablesUI(btn.tmp)
    )
    
    ## SAVE INPUTS FROM NUMBER params$btn INTO DATAFRAME
    
    observeEvent(input[[NS(btn.tmp, "variable")]], {
      add.variable$df[btn.tmp, 1] <- input[[NS(btn.tmp, "variable")]]
    })
    
    observeEvent(input[[NS(btn.tmp, "value")]], {
      add.variable$df[btn.tmp, 2] <- input[[NS(btn.tmp, "value")]] / 100
    })
    
  })
  
  #-------------------------- #
  # REMOVE VARIABLES
  
  observeEvent(input$removeVarBtn, {
    
    # REMOVE LAST LINE FROM DATAFRAME
    add.variable$df <- add.variable$df[-btn$value, ]
    
    # REMOVE LAST LINE MODULE UI
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#var', btn$value)
    )
    
    # SUBTRACT 1 FROM BUTTON VALUE 
    btn$value <- btn$value - 1
  })
  
  #-------------------------- #
  
  # OUTPUT DATAFRAME
  
  output$outDT <- renderFormattable({
    tbl <- add.variable$df
    tbl <- data.frame(
      Asset = tbl[,1],
      Share = percent(tbl[,2])
    )
    formattable(tbl,
                align = c("c", "c"),
                list(Asset = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                     Share = color_bar("lightblue")))
  })
  
  # ========================================== #
  # =========== Fetch Data ===================
  # ========================================== #
  # prices <- eventReactive(input$fetchData,{
  #   ASSET_SYMBOLS <- c(add.variable$df$variable)
  #   START_DATE <- input$start_date
  #   STOP_DATE <- input$end_date
  #   symbols_price <- get_symbols_price(
  #     symbols = ASSET_SYMBOLS,
  #     src = "yahoo",
  #     from = START_DATE, to = STOP_DATE,
  #     type = "Ad",
  #     auto.assign = TRUE,
  #     warnings = FALSE
  #   )
  #   return(symbols_price)
  # })
  
  # ========================================== #
  # =========== Returns ======================
  # ========================================== #
  asset_returns_xts <- eventReactive(input$fetchData, {
    ASSET_SYMBOLS <- add.variable$df$variable
    START_DATE <- as.Date(input$start_date)
    STOP_DATE <- as.Date(input$stop_date)
    #START_DATE <- as.Date("2015-12-31")
    #STOP_DATE <- as.Date("2020-12-31")
    symbols_price <- get_symbols_price(
      symbols = ASSET_SYMBOLS,
      src = "yahoo",
      from = START_DATE, to = STOP_DATE,
      type = "Ad",
      auto.assign = TRUE,
      warnings = FALSE
    )
    
    asset_returns <- 
      to_monthly_return_xts(symbols_price, indexAt = "lastof", method = "log")
    
    return(asset_returns)
  })
  
  output$asset_monthly_return <- renderHighchart({
    asset_returns <- asset_returns_xts()
    plot_monthly_return_hchart(asset_returns)
  })
}

shinyApp(ui, server)

