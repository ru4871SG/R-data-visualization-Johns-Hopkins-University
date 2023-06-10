#### Lubridate Examples

library(tidyverse)
#install.packages("lubridate")
library(lubridate)

#### There are three kinds of "temporal" data in the tidyverse

# A date
# A time
# A date-time, which is a time plus a date.

#### The most common usage of lubridate is to parse character strings into dates

#Take a character string that signifies a date, like "2020-01-01" and turn it into a date

my_dates<-c(
  "2020-01-01",
  "2020-01-02",
  "2020-01-03",
  "2020-01-04",
  
  "2020-02-01",
  "2020-02-02",
  "2020-02-03",
  "2020-02-04"
)

my_values<-c(
  1,2,3,4,5,6,7,8
)

dat<-tibble(my_dates,my_values)

dat$my_dates[4]-dat$my_dates[1]

####ymd() will parse the character string that falls into that format

dat$my_dates<-ymd(dat$my_dates)

####now we can do math with dates, and our plots will behave better

dat$my_dates[4]-dat$my_dates[1]

####other date parsers are mdy, myd, dmy, yq, and so forth.

#### Once you have data as a date, you can extract components of the date

dat$month<-month(my_dates)
dat$month
year(my_dates)
day(my_dates)
dat$wday<-wday(my_dates,label=TRUE)
dat$wday
#### As the documentation points out, putting your dates into date-time format helps with math. Resolves inconsistencies associated with leap days, February having fewer days, and the like. Be very cautious when manipulating dates mathematically.





