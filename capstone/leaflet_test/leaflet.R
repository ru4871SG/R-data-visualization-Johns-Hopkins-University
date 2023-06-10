library(shiny)
library(leaflet)
library(rgdal)
library(geojsonio)
library(tidyverse)

 
geo_data <- geojson_read("https://raw.githubusercontent.com/ru4871SG/US-states-geojson/main/stateswithoutpr.geo.json", what = "sp")

# Ensure that the GeoFips column is character
# This is to make sure that the merge operation later won't fail due to different data types
part2_map1$GeoFips <- as.character(part2_map1$GeoFips)

ui <- fluidPage(
  titlePanel("Income per Capita by state in the U.S."),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "selected_year", 
                  label = "Select Year:", 
                  min = 1970, 
                  max = 2022, 
                  value = 2022, 
                  step = 1,
                  #we need the sep otherwise we will see comma like 1,997 instead of 1997
                  sep = "")
    ),
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    # Filter data for inputID selected_year that we've defined in the sliderInput above
    data_for_year <- filter(part2_map1, year == input$selected_year)
    
    # Merge geo_data and data_for_year
    merged_data <- sp::merge(geo_data, data_for_year, by.x="fips", by.y="GeoFips")
    
    #let's define the map colors
    pal <- colorQuantile("Greens", NULL, n = 8)
    
    #mytext is for the hovertemplate data, which we will call under label later
    mytext <- paste(
      "<b>State:</b> ", merged_data@data$GeoName, "<br />",
      "<b>Income Per Capita:</b> ", sprintf("$%s", format(merged_data@data$income_per_capita, big.mark = ",")), "<br/>") %>%
      lapply(htmltools::HTML)
    
    #let's draw the map using leaflet
    leaflet(merged_data) %>%
      setView(lng = -110, lat = 40, zoom = 3) %>%
      addPolygons(
        fillColor = ~pal(income_per_capita), 
        fillOpacity = 0.7, 
        color = "#BDBDC3", 
        weight = 1, 
        label = ~mytext,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      )
  })
}

shinyApp(ui = ui, server = server)
