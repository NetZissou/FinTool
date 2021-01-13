library(shiny)
library(shinydashboard)
library(DT)
library(formattable)
# MODULE UI ----
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

#------------------------------------------------------------------------------#
# MODULE SERVER ----

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


portforlioUI <- function() {
  fluidRow(
    # Portfolio Table
    formattableOutput("outDT"),
    # MODULE UI FOR VARIABLE 1
    variablesUI(1),
    
    # MODULE UI FOR VARIABLE 2
    variablesUI(2),
    
    # Where to insert the other modules
    tags$div(id = 'placeholder'),
    
    # ADD / REMOVE BUTTONS
    fluidRow(
      column(width = 4, # ADD VARIABLE BUTTON
             actionButton(inputId = 'insertVarBtn', label = "Add New Asset", width = "100%")),
      column(width = 4, # REMOVE VARIABLE BUTTON
             actionButton(inputId = 'removeVarBtn', label = "Remove Last Asset", width = "100%")),
      column(width = 4, # Fetch Data
             actionButton(inputId = 'fetchData', label = "Fetch Data", width = "100%"))
    )
  )
}

portforlio_table <- function(box_width = 12, collapsible = F) {
  box(width = box_width, collapsible = collapsible,
      # Portfolio Table
      formattableOutput("outDT")
  )
}

asset_choice <- function(box_width = 12, collapsible = F) {
  box(solidHeader = T, title = "Modify Portforlio",
    width = box_width, collapsible = collapsible,
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
               actionButton(inputId = 'fetchData', label = "Fetch", width = "100%"))
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
             portforlio_table(),
             asset_choice(collapsible = T)
             )
    )
  )
)


server <- function(input, output, session) {
  
  # CREATE EMPTY DATAFRAME
  add.variable <- reactiveValues()
  
  add.variable$df <- data.frame(
    "variable" = character(0),
    "value" = numeric(0),
    stringsAsFactors = FALSE
  )
  
  #--------------------------
  # CALL MODULE 1 
  
  callModule(variablesServer, 1)
  
  ## SAVE INPUTS FROM 1 INTO DATAFRAME
  observeEvent(input[[NS(1, "variable")]], {
    add.variable$df[1, 1] <- input[[NS(1, "variable")]]
  })
  
  observeEvent(input[[NS(1, "value")]], {
    add.variable$df[1, 2] <- input[[NS(1, "value")]] / 100
  })
  
  #--------------------------
  # CALL MODULE 2
  callModule(variablesServer, 2)
  
  ## SAVE INPUTS FROM 2 INTO DATAFRAME
  
  observeEvent(input[[NS(2, "variable")]], {
    add.variable$df[2, 1] <- input[[NS(2, "variable")]]
  })
  
  observeEvent(input[[NS(2, "value")]], {
    add.variable$df[2, 2] <- input[[NS(2, "value")]] / 100
  })
  
  #--------------------------
  # START BUTTON VALUE AT 2 TO ACCOUNT FOR THE FIRST 2 VALUES
  
  btn <- reactiveValues(value = 2)
  
  #--------------------------
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
  
  #--------------------------
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
  
  #--------------------------
  
  # OUTPUT DATAFRAME
  
  # output$outDF <- renderPrint({
  #   print(add.variable$df)
  # })
  # output$outDT <- renderDataTable({
  #   datatable(add.variable$df)
  # })
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
  
}

shinyApp(ui, server)
