library(shiny)
library(tidyverse)

#####Import Data

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5"))
dat<-drop_na(dat)

ui<- fluidPage(
  
  verticalLayout(
    sliderInput("my_ideo",
                "Select Five Point Ideology (1=Very liberal, 5=Very conservative):",
                min = 1,
                max = 5,
                value = 3),
    plotOutput("congress_ideology")
  )
)

server<-function(input,output){
  
  output$congress_ideology <- renderPlot({
    ggplot(filter(dat,ideo5==input$my_ideo)) + 
      geom_bar(mapping=aes(x=pid7)) +
      coord_cartesian(ylim = c(0, 125), xlim=c(0, 8)) +
      scale_y_continuous(breaks = seq(0, 125, by = 25)) +
      scale_x_continuous(breaks = seq(0, 8, by = 2)) +
      xlab("7 Point Party ID, 1=Very D, 7=Very R")
  })
  
}

shinyApp(ui,server)
