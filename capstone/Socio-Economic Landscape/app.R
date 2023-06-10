library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(rgdal)
library(geojsonio)
library(janitor)

#####Import Data
# Source ----
source("helpers.R")

geo_data <- geojson_read("https://raw.githubusercontent.com/ru4871SG/US-states-geojson/main/stateswithoutpr.geo.json", what = "sp")

# Ensure that the GeoFips column is character
# This is to make sure that the merge operation later won't fail due to different data types
part2_map1$GeoFips <- as.character(part2_map1$GeoFips)

# ui
ui <- navbarPage(
  title="Socio-Economic Landscape",
  #first page
  tabPanel("Page 1",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 helpText("Scatterplot to see the correlation between income per capita vs. college completion and employment percentage. Choose the y-value to change the plot."),
                 selectInput(
                   #we use input$plot1_input in the server side
                   "plot1_input", h3("Select the y-value"),
                   choices = list(
                     "College Completion Percentage" = "collegecompletion_2017_2021", 
                     "Employment / Population" = "employment_percentage_2021"), 
                   selected = "collegecompletion_2017_2021")
               ),
               mainPanel(
                 plotlyOutput("plot1")
               )
             )
           )
  ),
  #second page
  tabPanel("Page 2",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 helpText("Interactive map to see income per capita level by state."),
                 sliderInput(inputId = "selected_year", 
                             label = "Select the Year:", 
                             min = 1970, 
                             max = 2022, 
                             value = 2022, 
                             step = 1,
                             #we need the sep otherwise we will see comma like 1,997 instead of 1997
                             sep = ""),
                 selectInput(inputId = "selected_geoName", 
                             label = "Filter by State:", 
                             choices = unique(part2_map1$GeoName), 
                             selected = unique(part2_map1$GeoName), 
                             multiple = TRUE)
               ),
               mainPanel(
                 leafletOutput(outputId = "map"),
                 plotlyOutput(outputId = "plot2", height = 750)#set the height here
               )
               
             )
           )
  ),
  #third page
  tabPanel("Page 3",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 helpText("Interactive map to see college completion by state."),
                 selectInput(
                   "selected_year2", h3("Select the Year"),
                   choices = list(
                     "1970" = "1970", 
                     "1980" = "1980",
                     "1990" = "1990",
                     "2000" = "2000",
                     "2008-2012" = "2012",
                     "2017-2021" = "2021"), 
                   selected = "2021"),
                 selectInput(inputId = "selected_geoName2", 
                             label = "Filter by State:", 
                             choices = unique(part2_map2$GeoName), 
                             selected = unique(part2_map2$GeoName), 
                             multiple = TRUE)
               ),
               mainPanel(
                 leafletOutput(outputId = "map2"),
                 plotlyOutput(outputId = "plot3", height = 750)#set the height here
               )
             )
           )
  ),
  #fourth page
  tabPanel("Page 4",
           fluidPage(
             mainPanel(
               fluidRow(
                 helpText("Line graphs to see the growth of income per capita and education level over time."),
                 plotlyOutput("plot4"),
                 tags$div(style = "display:inline-block; width:25px;"),
                 plotlyOutput("plot5")
               )
             )
           )
  )
)

# server
server <- function(input,output){
  
  output$plot1 <- renderPlotly({
    
    hover_label <- reactive({
      if (input$plot1_input == "collegecompletion_2017_2021") {
        return("College Completion")
      } else if (input$plot1_input == "employment_percentage_2021") {
        return("Employment / Population")
      } else {
        return(input$plot1_input)
      }
    })
    
    color_scale <- grDevices::colorRampPalette(viridis::viridis(100))
    
    part1_merged$color <- color_scale(100)[cut(part1_merged$incomepercapita_2021, breaks = 100)]
    
    plot_ly(data = part1_merged,
            x = ~incomepercapita_2021,
            y = ~part1_merged[[input$plot1_input]],
            text = ~paste("<b>State:</b>", GeoName,
                          "<br><b> Income per Capita:</b>", sprintf("$%s", formatC(incomepercapita_2021, format = "d", big.mark = ",")),
                          "<br><b>", hover_label(), ":</b>", sprintf("%.2f%%", 100 * part1_merged[[input$plot1_input]])),
            type = "scatter",
            mode = "markers",
            marker = list(size = 10, color = ~color),
            hoverinfo = "text") %>%
      layout(title = "Income Per Capita vs. Education and Employment",
             showlegend = FALSE,
             xaxis = list(title = "Income per Capita"),
             yaxis = list(title = hover_label(), tickformat = ".0%")) %>%
      #we need to add the lm line here due to plot_ly() difference with ggplot
      add_trace(x = ~incomepercapita_2021, y = ~lm(part1_merged[[input$plot1_input]] ~ incomepercapita_2021, data = part1_merged)$fitted.values, name = 'lm', type = 'scatter', mode = 'lines', line = list(color = "red", dash = "solid"), inherit=FALSE) %>%
      # add correlation coefficient as annotation
      add_annotations(x = 0.97, y = 0.1, text = paste0("Pearson's correlation coefficient: ", round(cor(part1_merged$incomepercapita_2021, part1_merged[[input$plot1_input]]), 4)), showarrow = FALSE, xref = "paper", yref = "paper")
  })
  
  # data_for_year as a reactive function for the second page
  data_for_year <- reactive({
    part2_map1 %>% 
      filter(year == input$selected_year & 
               GeoName %in% input$selected_geoName)
  })
  
  #first map
  output$map <- renderLeaflet({
    
    # Merge geo_data and data_for_year
    merged_data <- sp::merge(geo_data, data_for_year(), by.x="fips", by.y="GeoFips")
    
    #let's define the map colors
    pal <- colorQuantile("Greens", NULL, n = 5)
    
    #mytext is for the hovertemplate data, which we will call under label later
    mytext <- paste(
      "<b>State:</b> ", merged_data@data$GeoName, "<br />",
      "<b>Income Per Capita:</b> ", sprintf("$%s", format(merged_data@data$income_per_capita, big.mark = ",")), "<br/>") %>%
      lapply(htmltools::HTML)
    
    # Add a title to the first map
    titlemap1 <- tags$div(
      tags$h2(
        HTML("Income Per Capita Map"),
        style = "font-size: 16px; font-weight: bold;"
      )
    )
    
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
      )  %>%
      addLegend(
        pal = pal,
        values = ~ merged_data@data$income_per_capita,
        opacity = 0.8,
        position = "bottomright",
        title = "Income Per Capita Level %"
      ) %>%
      addControl(
        titlemap1,
        position = "topright"
      )
    })
  
  #first vertical bar graph
  output$plot2 <- renderPlotly({
    
    # Define green color theme
    green_theme <- theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#f4f0f0"),
        panel.background = element_rect(fill = "#c4e2c0"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
      )
    
    # Create bar plot
    ggplot1 <- ggplot(data_for_year(), 
                      aes(x = reorder(GeoName, income_per_capita), 
                          y = income_per_capita,
                          text = paste(GeoName, ":", sprintf("$%s", formatC(income_per_capita, format = "d", big.mark = ",")))
                          )) +
      geom_bar(stat = "identity", fill = "#428f61") +
      green_theme +
      coord_flip() +
      labs(title = "Vertical Bar Graph", x = "", y = "")
    
    ggplotly(ggplot1, tooltip = "text") %>%
      layout(hovermode = 'closest') %>% # Hover closest to cursor
      layout(
        hoverlabel = list(bgcolor = "white"
                          ))
  })
  
  # data_for_year2 as a reactive function for the third page
  data_for_year2 <- reactive({
    part2_map2 %>% 
      filter(year == input$selected_year2 & 
               GeoName %in% input$selected_geoName2)
  })
  
  #second map
  output$map2 <- renderLeaflet({
    
    # Merge geo_data and data_for_year2
    merged_data2 <- sp::merge(geo_data, data_for_year2(), by.x="fips", by.y="GeoFips")
    
    #let's define the map colors, let's use blue this time
    pal2 <- colorQuantile("Blues", NULL, n = 5)
    
    # Add a title to the second map
    titlemap2 <- tags$div(
      tags$h2(
        HTML("College Completion % Map"),
        style = "font-size: 16px; font-weight: bold;"
      )
    )
  
    mytext2 <- paste(
      "<b>State:</b> ", merged_data2@data$GeoName, "<br />",
      "<b>College Completion:</b> ", sprintf("%.2f%%", merged_data2@data$college_degree * 100), "<br/>") %>%
      lapply(htmltools::HTML)
    
    #let's draw the map using leaflet
    leaflet(merged_data2) %>%
      setView(lng = -110, lat = 40, zoom = 3) %>%
      addPolygons(
        fillColor = ~pal2(college_degree), 
        fillOpacity = 0.7, 
        color = "#BDBDC3", 
        weight = 1, 
        label = ~mytext2,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      addControl(
        titlemap2,
        position = "topright"
      ) 
  })
  
  #second vertical bar graph
  output$plot3 <- renderPlotly({
    
    # Define green color theme
    blue_theme <- theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#f4f0f0"),
        panel.background = element_rect(fill = "#c7d9e4"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
      )
    
    # Create bar plot
    ggplot2 <- ggplot(data_for_year2(), 
                      aes(x = reorder(GeoName, college_degree), 
                          y = college_degree,
                          text = paste(GeoName, ":", sprintf("%.2f%%", college_degree * 100)))
                      ) +
      geom_bar(stat = "identity", fill = "#487baf") +
      blue_theme +
      coord_flip() +
      labs(title = "Vertical Bar Graph", x = "", y = "")
    
    ggplotly(ggplot2, tooltip = "text") %>%
      layout(hovermode = 'closest') %>% # Hover closest to cursor
      layout(
        hoverlabel = list(bgcolor = "white"
        ))
  })
  

  #line graph for income per capita
  
  # Define a color palette with 8 colors from Viridis
  color_palette <- c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9D89FF", "#35B779FF", "#6DCD59FF")
  
  output$plot4 <- renderPlotly({
    
    #this is needed if we want to use the Viridis theme from color_palette above
    unique_geo_names <- unique(part4_incomeline$GeoName)
    color_map <- setNames(rep(color_palette, length.out = length(unique_geo_names)), unique_geo_names)
    
    plot_ly(data = part4_incomeline,
            type = "scatter", 
            mode = "lines",
            x = ~year, 
            y = ~income_per_capita, 
            color = ~GeoName, 
            colors = color_map,
            text = ~GeoName,
            hovertemplate = paste(
              "<b>%{text}<br></b>",
              "Income Per Capita: %{y:$,.0f}<br>",
              "Year: %{x:.0f}",
              "<extra></extra>"
            )
            ) %>% 
      layout(title = "Income Per Capita Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Income Per Capita"),
             dragmode = FALSE)
  })
  
  output$plot5 <- renderPlotly({
    
    unique_geo_names <- unique(part4_educationline$GeoName)
    color_map <- setNames(rep(color_palette, length.out = length(unique_geo_names)), unique_geo_names)
    
    plot_ly(data = part4_educationline,
            type = "scatter", 
            mode = "lines",
            x = ~year, 
            y = ~college_degree, 
            color = ~GeoName, 
            colors = color_map,
            text = ~GeoName,
            hovertemplate = paste(
              "<b>%{text}<br></b>",
              "College Completion: %{y:.2%}<br>",
              "Year: %{x:.0f}",
              "<extra></extra>"
            )
    ) %>% 
      layout(title = "College Completion Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "College Completion"),
             dragmode = FALSE)
  })
  
  
  
}

shinyApp(ui,server)
