library(shiny)
library(tidyverse)
library(urbnmapr)
library(plotly)

#this is to get the counties data from urbnmapr package
counties <- urbnmapr::counties

#we need the household income data by using left_join. let's show West Coast
household_data <- left_join(countydata, counties, by = "county_fips") %>% 
  filter(state_fips %in% c("06", "41", "53"))

# User interface ----
ui <- fluidPage(
  titlePanel("Median Household Income Map - West Coast"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("incomeRange",
                  "Median Household Income Range:",
                  #medhhincome is the median household income in the data frame
                  min = min(household_data$medhhincome, na.rm = TRUE),
                  max = max(household_data$medhhincome, na.rm = TRUE),
                  value = c(min(household_data$medhhincome, na.rm = TRUE), max(household_data$medhhincome, na.rm = TRUE))
      )
    ),
    mainPanel(
      plotlyOutput("householdMap", width = "100%", height = "100%")
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  output$householdMap <- renderPlotly({
    filtered_data <- household_data %>% 
      filter(medhhincome >= input$incomeRange[1] & medhhincome <= input$incomeRange[2])
    
    #this is to show dollar numbers in the legend, and not scientific numbers
    dollar_format <- function(x) {
      paste0("$", format(x, big.mark = ",", scientific = FALSE, trim = TRUE))
    }
    
    map <- filtered_data %>%
      ggplot(aes(long, lat, group = group, fill = medhhincome, 
                 text = paste(county_name, "<br>Median Income:", dollar_format(medhhincome)))) +
      geom_polygon(color = NA) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      scale_fill_gradient(low = "#132b3d", high = "#55b0f5", na.value = "transparent",
                          labels = dollar_format) +
      labs(fill = "Median Household Income")
    
    ggplotly(map, tooltip = "text") %>% 
      layout(hovermode = "closest") %>%
      config(displayModeBar = FALSE)
  })
  
}

# Run the app ----
shinyApp(ui, server)