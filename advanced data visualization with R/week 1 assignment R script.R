## Problem 1

library(tidyverse)

set.seed(12345)
var1<-runif(50,0,10)
set.seed(12345)
var2<-var1+rnorm(50,5,2)
set.seed(12345)
var3<- var1*(-rnorm(50,1,.2))

dat1<-tibble(var1,var2,var3)

ggplot(dat1, aes(x=var1,y= var2)) + geom_point() + geom_smooth() + ylab("Variable2") + xlab("Variable1")

## Problem 2

set.seed(10)
var1<-runif(50,0,10)
set.seed(10)
var2<-var1+rnorm(50,5,2)
set.seed(10)
var3<- var1*(-rnorm(50,1,.2))

dat2<-tibble(var1,var2,var3)

library(GGally)

my_scatter<-function(data,mapping){
  ggplot(data=data,mapping=mapping)+
    geom_jitter(color="orange")
}

my_density<-function(data,mapping){
  ggplot(data=data,mapping=mapping)+
    geom_density(fill="blue")
}

ggpairs(dat2,
        lower=list(continuous=my_scatter),
        diag=list(continuous=my_density))

## Problem 3

set.seed(12)
var1<-runif(50,0,10)
set.seed(12)
var2<-var1+rnorm(50,5,2)
set.seed(12)
var3<- var1*(-rnorm(50,1,.2))

dat3<-tibble(var1,var2,var3)

library(ggcorrplot)

r<-cor(dat3,use="complete.obs")

ggcorrplot(r,type="lower") + labs(title = "Correlations")


## Problem 4

set.seed(5678)
var1<-rnorm(10,25,5)
names<-letters[1:10]

dat4<-tibble(names,var1)

dat4_arranged <- dat4[order(dat4$var1, decreasing = FALSE),]

dat4_arranged$names <- factor(dat4_arranged$names, levels = unique(dat4_arranged$names))

# Create the plot
ggplot(dat4_arranged, aes(x = var1, y = names)) +
  geom_point() +
  xlab("Variable 1") +
  ylab("")


## Problem 5

set.seed(13)
var1<-rnorm(10,25,5)
names<-letters[1:10]

dat5<-tibble(names,var1)

ggplot(dat5,aes(x=reorder(names,var1),y=var1))+
  geom_point()+
  geom_segment(aes(x=names,xend=names,y=0,yend=var1))+
  theme(axis.text.x=element_text(angle=90)) + xlab("") + ylab("Variable 1")


## Problem 6

set.seed(8)
fiction<-tibble(Genre=rep("Fiction",5),Time=seq(1:5),Total=sample(1:10,5))
set.seed(7)
biography<-tibble(Genre=rep("Biography",5),Time=seq(1:5),Total=sample(1:10,5))
set.seed(9)
mystery<-tibble(Genre=rep("Mystery",5),Time=seq(1:5),Total=sample(1:10,5))

books_checked_out<-bind_rows(fiction,biography,mystery)

ggplot(books_checked_out,aes(x=Time,y=Total,fill=Genre)) + geom_area() + labs(title="Books Checked Out")


## Problem 7

books_checked_out2 <- books_checked_out %>%
  filter(Time==1 | Time==5) %>%
  pivot_wider(names_from = Time,values_from=Total) %>%
  rename(Time1=`1`,Time5=`5`)

library(ggalt)

ggplot(books_checked_out2,
       aes(y=reorder(Genre,Time1),
           x=Time1,
           xend=Time5))+
  geom_dumbbell(
    colour_x="purple", 
    colour_xend="red",
    size_x=3,
    size_xend=3
  )+
  labs(x="",y="Genre")


## Problem 8

pie_dat<-c(1,2,3,4,5)

library(RColorBrewer)

my_color <- c("red", "orange", "yellow", "green", "blue")

pie(pie_dat, col=my_color)