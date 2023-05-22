library(shiny)
library(tidyverse)
library(plotly)
library(DT)

#####Import Data

dat <- read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat <- dat %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
dat <- drop_na(dat)

# ui
ui <- navbarPage(
  title="My Application",
  #first page
  tabPanel("Page 1",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId="ideology_input",
                   label="Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
                   min=1,
                   max=5,
                   value=3,
                   sep="")
               ),
               mainPanel(
                 tabsetPanel(
                   #tab 1 and tab 2 in the first page
                   tabPanel("Tab1", plotOutput("plot1_1")), 
                   tabPanel("Tab2", plotOutput("plot1_2"))
                 )
               )
             )
           )
  ),
  #second page
  tabPanel("Page 2",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   inputId = "gender_input", 
                   label = "Select Gender", 
                   choices = c("1", "2"), 
                   selected = NULL)
                 ),
               mainPanel(
                 plotlyOutput("plot2")
               )
             )
           )
           ),
  #third page
  tabPanel("Page 3",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId="region_input",
                   label="Select Region",
                   choices = c("1", "2", "3", "4"),
                   selected= NULL,
                   multiple=TRUE
                 )
               ),
               mainPanel(
                 #we define the div style here so they won't be too big
                 div(style = 'height: 500px; overflow:auto;',
                     dataTableOutput("table3")
                 )
               )
             )
           )
          )
)

# server  
server <- function(input,output){
  
  #plot for page 1, 1st tab
  output$plot1_1 <- renderPlot({
    ggplot(filter(dat,ideo5==input$ideology_input)) + 
      geom_bar(mapping=aes(x=pid7)) +
      coord_cartesian(ylim = c(0, 100), xlim=c(0, 8)) +
      scale_y_continuous(breaks = seq(0, 100, by = 25)) +
      scale_x_continuous(breaks = seq(0, 8, by = 2)) +
      xlab("7 Point Party ID, 1=Very D, 7=Very R")
  })
  
  #plot for page 1, 2nd tab
  output$plot1_2 <- renderPlot({
    ggplot(filter(dat,ideo5==input$ideology_input)) + 
      geom_bar(mapping=aes(x=CC18_308a)) +
      coord_cartesian(xlim=c(0, 5)) +
      scale_x_continuous(breaks = seq(0, 5, by = 1)) +
      xlab("Trump Support")
  })
  
  #scatter plot with plotly for page 2
  output$plot2 <- renderPlotly({
    selected_gender <- input$gender_input
    filtered_dat <- dat %>% filter(gender %in% selected_gender)
    
    plot_with_jitter <- ggplot(data=filtered_dat, aes(x=educ, y=pid7)) +
      geom_jitter() +
      geom_smooth(method="lm")
    
    ggplotly(plot_with_jitter)
  })
  
  #data table for page 3
  output$table3 <- renderDT({
    datatable(dat[dat$region %in% input$region_input, ])
  })
}

shinyApp(ui,server)
