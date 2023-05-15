# Load packages ----
library(shiny)
library(quantmod)
library(plotly)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("stockVis"),

  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine.

        Information will be collected from Yahoo finance."),
      textInput("symb", "Symbol", "SPY"),

      dateRangeInput("dates",
                     "Date range",
                     start = "2013-01-01",
                     end = as.character(Sys.Date())),

      br(),
      br(),

      checkboxInput("log", "Plot y axis on log scale",
                    value = FALSE),

      checkboxInput("adjust",
                    "Adjust prices for inflation", value = FALSE)
    ),

    mainPanel(plotlyOutput("plot"))
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  finalInput <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())
  })
  
  output$plot <- renderPlotly({
    data_xts <- finalInput()
    
    # Convert xts object to a data frame
    data_df <- data.frame(Date = index(data_xts), coredata(Cl(data_xts)))
    colnames(data_df) <- c("Date", "Price")
    
    # Create a plotly interactive plot
    plot_ly(data_df, x = ~Date, y = ~Price, type = "scatter", mode = "lines") %>%
      layout(yaxis = list(type = ifelse(input$log, "log", "linear")))
  })
  
}

# Run the app
shinyApp(ui, server)
