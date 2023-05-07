library(tidyverse)

Category<-c("Alpha","Beta","Zeta")
City<-c("Hong Kong","London","Nairobi")

my_dat<-expand_grid(Category,City)

set.seed(84684)

my_dat$Value<-sample(1:10,9,replace=T)

library(gganimate)
library(gifski)

problem_1_anim <- ggplot(my_dat, aes(x = Category, y = Value, fill = City)) +
  geom_bar(stat = "identity", position = "dodge") + labs(title = "Problem 1") +
  transition_states(
    City, transition_length = 2, state_length = 1
  ) +
  enter_fade() +
  exit_fade()

#the result
problem_1_anim


## Problem 2

Response<-c("Energize","Amazing","Great")
set.seed(9819)
Energize<-tibble(Company=rep("Energize",100),Output=rnorm(100,50,20))
set.seed(9819)
Amazing<-tibble(Company=rep("Amazing",100),Output=rnorm(100,50,10))
set.seed(9819)
Great<-tibble(Company=rep("Great",100),Output=rnorm(100,40,5))

my_dat2<-bind_rows(Energize,Amazing,Great)

library(plotly)

ggplotly(ggplot(my_dat2,aes(x=Company,y=Output, fill=Company))+
           geom_boxplot() + labs(title = "Problem 2")
)


## Problem 3

Category<-seq(from=1,to=10)
Time<-seq(from=1,to=10)

dat3<-expand_grid(Category,Time)

set.seed(78957)
dat3$Quantity<-runif(100,0,10)

ggplotly(ggplot(dat3,aes(x=Category,y=Quantity,frame=Time))+
           geom_point()+
           geom_segment(aes(x=Category,xend=Category,y=0,yend=Quantity))+
           theme(axis.text.x=element_text(angle=90)) + labs(title = "Problem 3"))
