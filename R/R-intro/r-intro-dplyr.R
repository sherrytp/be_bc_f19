# author: @lrdegeest

# https://www.tidyverse.org/
# https://dplyr.tidyverse.org/
library(tidyverse)

# manipulation with dplyr --------------------------------------------------
# dplyr is a jewel in the R data science crown
# it is bundled with the tidyverse
# it takes the idea that objects are nouns and functions are verbs very seriously
# the main purpose of dplyr is to make the manipulation of data frames highly literate
# code is written in a way that resembles a written sentence, in which words are chained 
#  together to create ideas
# in dply, functions are chained together to create summaries of dataframes

# load data
data("mpg")
df <- mpg

## EXAMPLE
# in English, we might think to ourselves: 
# "take the dataset THEN
#   make a new data set of certain variables THEN
#     summarize the new data set by calculating the mean and standard deviation"

# in dplyr: 
df %>% 
  select(cty, hwy) %>% 
  summarise_all(funs(mean,sd))

# notice the output of each line is "pipped" with %>% ("THEN") to the next line
# this makes it very easy - and very legible - to express and perform complex operations on data

# example: summarize multiple columns by group
df %>%
  group_by(class) %>%
  select(cty, hwy) %>% 
  summarise_all(funs(mean, sd)) 

# example: filter out certain rows, then summarize multiple columns by group
df %>%
  group_by(class) %>%
  select(cty, hwy) %>% 
  filter(cty < 25) %>%   
  summarise_all(funs(mean, sd)) 

# example: filter out certain rows, summarize multiple columns by multiple groupings and 
#   arrange the output by one grouping
df %>% # take the data THEN
  group_by(class, year) %>% # group observations by class and year THEN
  select(cty, hwy) %>%  # select the variables cty and hwy THEN
  filter(cty < 25) %>%  # keep observations of cty that are less than 25 THEN
  summarise_all(funs(mean, sd)) %>% # calculate the mean and sd for cty and hwy within class and year THEN
  arrange(year) # arrange the output in ascending order by year

# example: create a new column with mutate()
# example: calcuate the z-score for each cty observation within a class and year
x <- df %>%
  group_by(class,year) %>% 
  select(class,year,cty) %>% 
  mutate(z = (cty - mean(cty))/sd(cty))

# you can store the output in a data frame:
zscores <- df %>%
  group_by(class,year) %>% 
  select(class,year,cty) %>% 
  mutate(z = (cty - mean(cty))/sd(cty))
head(zscores)

# or you can send it straight to a plot:
df %>%
  group_by(class,year) %>% 
  select(class,year,cty) %>% 
  mutate(z = (cty - mean(cty))/sd(cty)) %>% 
  qplot(cty, data = ., geom="density", facets = year~class)

# Review
# dplyr makes it very easy to manipulate any data frame with the functions:
#   select()
#   group_by()
#   filter()
#   mutate()