####Using stringr, part of the tidyverse
library(tidyverse)

####create an example string

my_strings<-c(
  "123apple",
  " 456apple",
  "358orange   "
)

#### logical test for whether a string contains a pattern
str_detect(my_strings,"orange")

#### this will return the index positions of the value inside "my_strings"
str_which(my_strings,"apple")
str_which(my_strings,"3")

#### count the number of matches in a string

str_count(my_strings,"p")

#### indicate the positions of a pattern in a string

str_locate(my_strings,"apple")

#### extract a substring from a character vector

str_sub(my_strings,start=4,end=6)

#### return the strings in a vector of strings that contain a pattern

str_subset(my_strings,"apple")

#### return a specified pattern from all strings in a vector containing the pattern

str_extract(my_strings,"apple")

#### return the length of strings

str_length(my_strings)

#### add blank character spaces up to given number

str_pad(my_strings,12,side="right")

#### remove blank space in a vector from the left and/or right sides

my_strings<-str_trim(my_strings,"both") 

#### substitute a range of characters in a strings with new values 

str_sub(my_strings,1,3)<-"___"
my_strings

#### replace one pattern in strings with another pattern

str_replace(my_strings,"apple","grape")
###see also str_replace_all

#### change case of letters in strings
str_to_upper(my_strings)
#see also str_to_lower and str_to_title

#### joining strings across columns

my_strings1<-c(
  "app","gra"
)
my_strings2<-c(
  "le","pe"
)

joining_strings<-tibble(my_strings1,my_strings2)

str_c(joining_strings$my_strings1,joining_strings$my_strings2)

#### joining strings down rows

my_strings<-c(
  "app","le"
)

str_c(my_strings,collapse="")

#### split a string at a given pattern match

my_strings<-c(
  "appleX123",
  "orangeX456",
  "tomatoX789")
my_strings

####  create a list with the split substrings
str_split(my_strings,"X")

#### create a matrix of substrings

str_split_fixed(my_strings,"X",2)

#### be careful with regular expressions

my_strings<-c(
  "apple.123",
  "orange.456",
  "tomato.789")

str_split(my_strings,"\\.")

