#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tools)
library(stringr)


source("../www/Model_Fun.R")
source("../www/PresentValue.R")
source("../www/Mode.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Deterministic Age Structured Bio-economic Model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "NumYears", 
                   label = "Years (total)", 
                   min = 2, max = 100, 
                   step = 1,
                   value = 20),
      
      sliderInput("Lc",
                  "Length at first capture:",
                  min = 1,
                  max = 125,
                  value = 32),
      
      numericInput(inputId = "F.mort", 
                   label = "Effort", 
                   min = 0.258, max = 1, 
                   step = 0.001,
                   value = 0.5706),
      
      sliderInput("months.open",
                  "Seasonal closure (months open)",
                  min = 1,
                  max = 12,
                  value = 12),
      
      sliderInput("recovery",
                  "Long-term Closure (years)",
                  min = 0,
                  max = 19,
                  value = 0),
      
      numericInput(inputId = "delta", 
                   label = "Discount Rate", 
                   min = 0, max = 1, 
                   step = 0.005,
                   value = 0.09),
      
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Profit (2012 USD)" = "Profit", 
                              "Biomass (Kg)" = "Biomass", 
                              "Catch (Kg)" = "CatchTotal", 
                              "Revenue (2012 USD)" = "RevenueTotal", 
                              "Year" = "year"), 
                  selected = "Profit"),
      
      # Select variable for x-axis --------------------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Profit (2012 USD)" = "Profit", 
                              "Biomass (Kg)" = "Biomass", 
                              "Catch (Kg)" = "CatchTotal", 
                              "Revenue (2012 USD)" = "RevenueTotal", 
                              "Year" = "year"), 
                  selected = "year"),
      
      actionButton(inputId = "recalc", 
                   label = "Calculate")
    
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput(outputId = "NPV"),
      HTML("<br>"),
      
      wellPanel(plotOutput("Profitplot")),
      
      # Show data table -------------------------------------------------------
      wellPanel(DT::dataTableOutput(outputId = "Outtable"))
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  Model.full <- eventReactive(eventExpr = input$recalc,
                                 valueExpr = {
                                   
                                   Model_fun(c(input$F.mort, input$Lc, input$months.open, input$recovery, input$NumYears))
                                 
                                   },
                                 ignoreNULL = FALSE
  )
  
  
  
  # Model.full <- reactive({
  #   
  #   Model_fun(c(input$F.mort, input$Lc, input$months.open, input$recovery, input$NumYears))
  #   
  # })
  
  tableDATA  = reactive({ 
    
    Model.full() %>%
    select(Months.closed , F.Mortality, Selectivity, year, CatchTotal, RevenueTotal, Biomass, Profit) %>%
      rename(F = F.Mortality, MonthsClosed = Months.closed, Catch = CatchTotal, Revenue = RevenueTotal )
  
    
  })
  
  #colnames(tableDATA()) = c('Months Closed', 'F', 'Selectivity', "Year", "Catch", "Revenue", "Biomass", "Profit")
  
  NPV = reactive({ 
    Model.full() %>%
    summarise(NPV = sum(PresentValue(Profit, discount = input$delta, year)))

  })
  
  output$Outtable <- DT::renderDataTable({
    
    
    DT::datatable(data = tableDATA(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE) %>%
     DT::formatRound(columns=c( 'F',  "Catch", "Revenue", "Biomass", "Profit"), digits=3)
  
    })
  
  
  #Plot of selected spp-----
    
    output$Profitplot <- renderPlot({
      # generate bins based on input$bins from ui.R
      
      ggplot(Model.full(),aes_string(x = input$x, y = input$y)) +
      #ggplot(Model.full(), aes( year, (Profit/1000))) +
        geom_hline(yintercept = 0, lty = 2) +
        geom_line( size =1 ) +
        #scale_y_continuous(breaks = c(seq(-100,4000,600))) +
        scale_x_continuous(breaks = c(seq(0,20, 2))) +
        labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
             y = toTitleCase(str_replace_all(input$y, "_", " "))) +
        #labs(x = "Time (years)", y = "Real Profits") +
        theme_minimal()
      
      
    })
  
  output$NPV <- renderText({
    paste("Net Present Value:", round((NPV()/1000), 1), "thousand USD")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

