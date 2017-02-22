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

source("../www/Model_Fun.R")
source("../www/PresentValue.R")
source("../www/Mode.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Deterministic Age Structured Model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Lc",
                  "Length at first capture:",
                  min = 1,
                  max = 125,
                  value = 32),
      
      numericInput(inputId = "F.mort", 
                   label = "Effort", 
                   min = 0.258, max = 1, 
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
      
      
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Profit" = "Profit", 
                              "Biomass" = "Biomass", 
                              "Catch" = "CatchTotal", 
                              "Revenue" = "RevenueTotal", 
                              "Year" = "year"), 
                  selected = "Profit"),
      
      # Select variable for x-axis --------------------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Profit" = "Profit", 
                              "Biomass" = "Biomass", 
                              "Catch" = "CatchTotal", 
                              "Revenue" = "RevenueTotal", 
                              "Year" = "year"), 
                  selected = "year")
      
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
  
  
  
  
  
  Model.full <- reactive({
    
    Model_fun(c(input$F.mort, input$Lc, input$months.open, input$recovery))
    
  })
  
  tableDATA  = reactive({ 
    
    Model.full() %>%
    select(Months.closed , F.Mortality, Selectivity, year, CatchTotal, RevenueTotal, Biomass, Profit) %>%
      rename(F = F.Mortality, MonthsClosed = Months.closed, Catch = CatchTotal, Revenue = RevenueTotal )
  
    
  })
  
  #colnames(tableDATA()) = c('Months Closed', 'F', 'Selectivity', "Year", "Catch", "Revenue", "Biomass", "Profit")
  
  NPV = reactive({ 
    Model.full() %>%
    summarise(NPV = sum(PresentValue(Profit, discount = 0.09, year)))

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
        labs(x = "Time (years)", y = "Real Profits (thousand USD)") +
        theme_minimal()
      
      
    })
  
  output$NPV <- renderText({
    paste("Net Present Value:", round((NPV()/1000), 1), "thousand USD")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

