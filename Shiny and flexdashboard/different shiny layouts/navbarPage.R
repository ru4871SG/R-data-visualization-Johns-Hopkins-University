library(shiny)
library(tidyverse)

cities=c("Hong Kong","Macau","Dubai")

city1 <- data.frame("city"=rep(cities[1],5),
                    "year"=seq(from=1990,to=1994,by=1),
                    "var1"=runif(5,0,5),
                    "var2"=runif(5,0,5),
                    "var3"=runif(5,0,5))
city2 <- data.frame("city"=rep(cities[2],5),
                    "year"=seq(from=1990,to=1994,by=1),
                    "var1"=runif(5,0,5),
                    "var2"=runif(5,0,5),
                    "var3"=runif(5,0,5))
city3 <- data.frame("city"=rep(cities[3],5),
                    "year"=seq(from=1990,to=1994,by=1),
                    "var1"=runif(5,0,5),
                    "var2"=runif(5,0,5),
                    "var3"=runif(5,0,5))

all_data <- bind_rows(city1,city2,city3)

ui <- navbarPage(
  title="My Application",
  tabPanel("Component 1",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId="year_input",
                   label="Year",
                   min=1990,
                   max=1994,
                   value=1990,
                   sep="")
               ),
               mainPanel(
                 plotOutput("plot1")
               )
             )
           )
  ),
  tabPanel("Component 2",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId="year_input2",
                   label="Year",
                   min=1991,
                   max=1993,
                   value=1991,
                   sep="")
               ),
               mainPanel(
                 plotOutput("plot2")
               )
             )
           )
  ),
  tabPanel("Component 3 - Empty")
)

server <- function(input,output){
  output$plot1 <- renderPlot({
    plot_dat <- filter(all_data, year == input$year_input)
    ggplot(plot_dat, aes(x=city, y=var1, fill=city)) + geom_bar(stat="identity")
  })
  
  output$plot2 <- renderPlot({
    plot_dat2 <- filter(all_data, year == input$year_input2)
    ggplot(plot_dat2, aes(x=city, y=var1, fill=city)) + geom_bar(stat="identity")
  })
}

shinyApp(ui=ui, server=server)
