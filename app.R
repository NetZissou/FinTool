# ======================================================================= #
# ========================= Environment Settings ========================
# ======================================================================= #
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
source("initialize.R")
INITIAL_ASSET_CHOICES <- c("SPY", "EFA", "IJS", "EEM", "AGG")
INITIAL_ASSET_WEIGHTS <- c(0.25, 0.25, 0.2, 0.2, 0.1)
# ======================================================================= #
# ========================= UI Elements =================================
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
               #actionButton(inputId = 'insertVarBtn', label = "Add", width = "100%")
               actionBttn(
                 inputId = "insertVarBtn",
                 label = "Add",
                 style = "jelly", 
                 color = "primary"
               )),
        column(width = 4, # REMOVE VARIABLE BUTTON
               #actionButton(inputId = 'removeVarBtn', label = "Remove", width = "100%")
               actionBttn(
                 inputId = "removeVarBtn",
                 label = "Remove",
                 style = "jelly", 
                 color = "primary"
               )),
        column(width = 4, # Fetch Data
               #actionButton(inputId = 'fetchData', label = "Fetch/Update", width = "100%")
               actionBttn(
                 inputId = "fetchData",
                 label = "Fetch/Update",
                 style = "jelly", 
                 color = "danger"
               ))
      )
  )
}

# Return -------------------------------------------------------------- #
asset_return_hchart <- function(box_width = 6) {

  box(width = box_width, 
      highchartOutput("asset_monthly_return"))
}

portfolio_return_hchart <- function(box_width = 6) {

  box(width = box_width, 
      highchartOutput("portfolio_monthly_return"))
}

# Risk -------------------------------------------------------------- #
sd_overtime_hchart <- function(box_width = 6) {
  
  box(width = box_width, 
      highchartOutput("sd_overtime"))
}

sd_comparision_hchart <- function(box_width = 6) {
  
  box(width = box_width, 
      highchartOutput("sd_comparison"))
}

return_vs_risk_hchart <- function(box_width = 6) {
  
  box(width = box_width, 
      highchartOutput("return_vs_risk"))
}

rolling_volatility_hchart <- function(box_width = 6) {
  
  box(width = box_width, 
      highchartOutput("rolling_sd"))
}

# Skewness ---------------------------------------------------------- #
skew_density_hchart <- function(box_width = 6) {
  
  box(width = box_width, 
      highchartOutput("skew_density"))
}

skew_comparison_hchart <- function(box_width = 6) {
  
  box(width = box_width, 
      highchartOutput("skew_comparison"))
}

rolling_skew_hchart <- function(box_width = 6) {
  
  box(width = box_width, 
      highchartOutput("rolling_skew"))
}
# Kurtosis ----------------------------------------------------------- #
kurtosis_density_hc <- function(box_width = 6) {
  box(width = box_width, 
      highchartOutput("kurtosis_density"))
}

kurtosis_comparison_hchart <- function(box_width = 6) {
  box(width = box_width, 
      highchartOutput("kurtosis_comparison"))
}

rolling_kurtosis_hchart <- function(box_width = 6) {
  box(width = box_width, 
      highchartOutput("rolling_kurtosis"))
}

# ======================================================================= #
# ========================== Shiny UI ===================================
# ======================================================================= #

ui <- dashboardPage(
  dashboardHeader(titleWidth = 350, 
                  title = "Portfolio Terminal Lite"
                    # shinyDashboardLogo(
                    # theme = "flat_red",
                    # boldText = "Portfolio Terminal",
                    # mainText = "Lite",
                    # badgeText = "v1.1"
                    # )
                  ),
  dashboardSidebar(
    disable = T
  ),
  dashboardBody(
    # theme
    # shinyDashboardThemes(
    #   theme = "flat_red"
    # ),
    fluidRow(
      # column(width = 3,
      #        portfolio_table(),
      #        asset_choice(collapsible = T)
      #        ),
      column(width = 1,
             dropdownButton(
               portfolio_table(),
               asset_choice(collapsible = T),
               circle = TRUE, status = "info",
               icon = icon("folder", lib = "font-awesome"), width = "700px",
               
               tooltip = tooltipOptions(title = "Manage Portfolio")
              ),
             # ------------ break -------------------------------- #
             br(),
             br(),
             dropdownButton(
               h3("word"),
               circle = TRUE, status = "info",
               icon = icon("gear"), width = "700px",
               
               tooltip = tooltipOptions(title = "Manage Portfolio")
              )
             ),
      column(width = 5,
             tabBox(
               title = "Returns",
               # The id lets us use input$tabset1 on the server to find the current tab
               id = "tabset1", height = "100%", width = "100%",
               tabPanel("Asset Returns", 
                        fluidRow(
                          asset_return_hchart(box_width = 12)
                          )
                        ),
               tabPanel("Portfolio Returns", 
                        fluidRow(
                          portfolio_return_hchart(box_width = 12)
                          )
                        )
              )
             ),
      column(width = 6,
             tabBox(
               title = "Risks",
               # The id lets us use input$tabset1 on the server to find the current tab
               id = "tabset2", height = "100%", width = "100%",
               tabPanel("Votility", 
                        fluidRow(
                          # column(width = 6, sd_overtime_hchart()),
                          # column(width = 6, 
                          #        return_vs_risk_hchart(),
                          #        sd_comparision_hchart()
                          #        )
                          return_vs_risk_hchart(box_width = 7),
                          sd_comparision_hchart(box_width = 5),
                          sd_overtime_hchart(box_width = 12)
                          # box(width = 12, rolling_volatility_hchart())
                          )
                        ),
               tabPanel("Skewness", 
                        fluidRow(
                          skew_density_hchart(box_width = 7),
                          skew_comparison_hchart(box_width = 5),
                          rolling_skew_hchart(box_width = 12)
                          )
                        ),
               tabPanel("Kurtosis",
                        fluidRow(
                          kurtosis_density_hc(box_width = 7),
                          kurtosis_comparison_hchart(box_width = 5),
                          rolling_kurtosis_hchart(box_width = 12)
                          )
                        )
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
      value = INITIAL_ASSET_CHOICES[round(runif(1, 1, 5))], # Randomly initialization
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
  #CALL MODULE 1

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
  
  asset_weights <- eventReactive(input$fetchData, {
    ASSET_WEIGHTS <- add.variable$df$value
    return(ASSET_WEIGHTS)
  })
  
  portfolio_returns_xts <- reactive({
    asset_returns <- asset_returns_xts()
    ASSET_WEIGHTS <- asset_weights()
    portfolio_returns <- 
      to_portfolio_returns_xts(asset_returns, ASSET_WEIGHTS, rebalance_mode = "months")
    return(portfolio_returns)
  })
  
  output$asset_monthly_return <- renderHighchart({
    asset_returns <- asset_returns_xts()
    plot_monthly_return_hchart(asset_returns)
  })
  
  output$portfolio_monthly_return <- renderHighchart({
    portfolio_returns <- portfolio_returns_xts()
    plot_monthly_return_hchart(portfolio_returns, title = "Portfolio Monthly Log Returns")
  })
  
  # ========================================== #
  # =========== Risks ========================
  # ========================================== #
  
  # SD ----------------------------------------------------------------------------#
  output$sd_overtime <- renderHighchart({
    portfolio_returns <- portfolio_returns_xts()
    plot_sd_overtime_hc(portfolio_returns)
  })
  
  output$sd_comparison <- renderHighchart({
    asset_returns <- asset_returns_xts()
    ASSET_WEIGHTS <- asset_weights()
    plot_sd_comparison_hc(asset_returns, ASSET_WEIGHTS)
  })
  
  output$return_vs_risk <- renderHighchart({
    asset_returns <- asset_returns_xts()
    ASSET_WEIGHTS <- asset_weights()
    plot_return_vs_risk_hc(asset_returns, ASSET_WEIGHTS)
  })
  
  # output$rolling_sd <- renderHighchart({
  #   portfolio_returns <- portfolio_returns_xts()
  #   portfolio_rolling_sd <- get_portfolio_rolling_sd(portfolio_returns, window = 24)
  #   plot_rolling_volatility_hc(portfolio_rolling_sd)
  # })
  
  # Skewness -------------------------------------------------------------------------#
  output$skew_density <- renderHighchart({
    
    portfolio_returns <- portfolio_returns_xts()
    plot_skew_density_hc(portfolio_returns)
  })
  
  output$skew_comparison <- renderHighchart({
    
    asset_returns <- asset_returns_xts()
    ASSET_WEIGHTS <- asset_weights()
    plot_skew_comparison_hc(asset_returns, ASSET_WEIGHTS)
  })
  
  output$rolling_skew <- renderHighchart({
    
    portfolio_returns <- portfolio_returns_xts()
    portfolio_rolling_skew <- get_portfolio_rolling_skew(portfolio_returns, window = 24)
    plot_rolling_skew_hc(portfolio_rolling_skew)
  })
  
  # Kurtosis -------------------------------------------------------------------------#
  output$kurtosis_density <- renderHighchart({
    
    portfolio_returns <- portfolio_returns_xts()
    plot_kurtosis_density_hc(portfolio_returns)
  })
  
  output$kurtosis_comparison <- renderHighchart({
    
    asset_returns <- asset_returns_xts()
    ASSET_WEIGHTS <- asset_weights()
    plot_kurtosis_comparison_hc(asset_returns, ASSET_WEIGHTS)
  })
  
  output$rolling_kurtosis <- renderHighchart({
    
    portfolio_returns <- portfolio_returns_xts()
    portfolio_rolling_kurtosis <- get_portfolio_rolling_kurtosis(portfolio_returns, window = 24)
    plot_rolling_kurtosis_hc(portfolio_rolling_kurtosis)
  })
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)

