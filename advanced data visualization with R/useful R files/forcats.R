#### Forcats

library(tidyverse)

# A factor is a categorical variable in which the factors have some kind of relationship with each other.

# Imagine a dataset that looks like this:

groups<-rep(c("GroupA","GroupB","GroupC"),2)
time<-c(1,1,1,2,2,2)
relative_score<-c("High","Medium","Low","Low","High","Medium")
exact_score<-c(3,2,1,1,3,2)

dat2<-tibble(groups,time,relative_score,exact_score)

# Score is a categorical variable. It describes three different conditions, but they are numbers. They have relative values, but not numerically. Right now, the column "score" is a character vector, but this loses the relative value.

# Let's make this a factor

dat2$relative_score<-factor(dat2$relative_score)

# Now we see that the score column is a factor. To determine the "levels" of the factor or their values relative to each other, let's just call up that column.

dat2$relative_score

# This output shows that there are now "levels" to the factor, which indicate their relative values. However, right now, those factors are in the wrong order. We need to "relevel" the factor.

dat2$relative_score <- fct_relevel(dat2$relative_score,"Low","Medium","High")

# There are lots of quick functions for changing the order of the factors like this. See the cheat sheet for more examples. However, one of the most useful is fct_reorder, which allows you to reorder a factor based on the levels' relationship with another variable.

# Consider this example:

groups<-c("c","d","e","b","a")
values<-c(5,2,3,4,1)

dat<-tibble(groups,values)

dat %>% 
  ggplot(aes(x=groups,y=values))+
  geom_bar(stat="identity")

### By default, ggplot is organizing the bars alphabetically, but you might want to sort the bars in the ascending or descending value. The easier way to do is the reorder the groups variable as a factor.

#revel groups ascending by values. no need to use factor() before fct_reorder, fct_reorder automatically does it.
dat$groups<-fct_reorder(dat$groups,dat$values)

dat %>% 
  ggplot(aes(x=groups,y=values))+
  geom_bar(stat="identity")

#or by descending if you prefer
dat$groups<-fct_reorder(dat$groups,desc(dat$values))

dat %>% 
  ggplot(aes(x=groups,y=values))+
  geom_bar(stat="identity")
       