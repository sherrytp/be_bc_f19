# author: @lrdegeest

# Overview ----------------------------------------------------------------
# Working in R boils down to three tasks:
## 1. Creating objects (e.g. data)
## 2. Applying functions to those objects (e.g. run a regression on the data)
## 3. Using iteration to scale tasks 1 and 2 
### advice: first work through 1 and 2, then come back for 3

# 1. Objects --------------------------------------------------------------
# objects are nouns
number <- 1 # vector with one number observation
letter <- "a" # vector with one string observation
# objects are mutable
number <- number + 1
letter <- "b"

# most common objects that we will use are vectors, matrices and dataframes
x <- c(1,2,3) # vector with three numeric observations
y <- c("a", "b", "c") # vector with three string observations
df <- data.frame(x,y) # dataframe: an object containing many vectors with some added perks
matrix(nrow=2,ncol=2) # empty 2x2 matrix
matrix(0,nrow=2,ncol=2) # matrix of zeros
matrix(runif(10), 2,2) # matrix of random uniform numbers between [0,1]
diag(2) # 2x2 identity matrix

# 2. Functions ------------------------------------------------------------
# functions are verbs that manipulate objects
# like in math, a function is machine that takes an argument(s) and returns something

# sample data
x <- c(1,2,3,4,5)

# mean() takes a vector as an argument and returns the mean of that vector
mean(x)

# you can write your own functions
# example: write a function "JointSum(x,y)" that takes two any two numbers "x" and "y" and returns the square of their product divided by their sum
JointSum <- function(x,y){
  # code here
}
# test it: JointSum(2,3) should return 7.2

# example: write a function that calculates the mean
get_mean <- function(data){
  sum = sum(data)
  n = length(data)
  average = sum / n
  return(average)
}

# check if it works
get_mean(x)

# 3. Dataframes -----------------------------------------------------------
library(tidyverse) # libraries are bundles of functions that you bring into your workspace
# if you don't have tidyverse you have to install it like this: install.packages("tidyverse")

# use built-in data from tidyverse
data("mpg")
df <- mpg

# basic commands to view raw data
head(df)
head(df,5)
tail(df,15)
View(df)
dim(df)
nrow(df)
names(df)

# you can index dataframes with $
# the data frame is made up of vectors which you access using "$"
class(df$mpg)
# we can runb functions over these vectors
mean(df$mpg) # mean miles per gallon
sd(df$mpg) # standard deviation miles per gallon

# basic stats: sum(), mean(), sd(), ... 
sum(df$cyl)
mean(df$cyl)
median(df$cyl)
sd(df$cyl)
summary(df$cyl)
table(df$year, df$class) # contingency table

# you can slice data frames by index: df[row, column]
df[1,] # first row
df[,1] # first column
df[2,3] # 2nd row, 3rd column
# or by name: (this only applies to columns)
df['model']
# you can save slices to new dataframes ('subsetting')
df2 <- df[1:20,] # save the first ten rows. same as head(df,10)
# or with the subset function, which is more flexible
df3 <- subset(df, model == 'a4')

#3. iteration -----------------------------------------------------
# iteration means looping
# loops allow you to easily do the same task many times
# here is a simple loop: 
for(i in 1:5) { # for each number 1,2,3,4,5:
  # the loop creates a value for i; here it starts at 1 and ends at 5
  # do something with i, e.g. print the number
  print(i)
}
# you always give something to "loop over": a starting point and an end point (above it was 1 to 5)
# the important thing is that you can loop over data
# example: 
x <- c(1,2,3,4,5) # here is a vector of data: 1,2,3,4,5
for(i in x) { # for each element of the data x:
  print(i+1) # print the element plus 1
}
# or:
for(i in 1:length(x)) print(i) + 1
# two differences with this loop over the previous one:
# 1. I iterated over the *length* of x. insight: each element of x has a position in the vector
# 2. If you keep the code on one line you don't need brackets

# you can loop over dataframes
## this loops over the columns
df_loop <- head(df,5)
for(i in df_loop) print(class(i))
for(i in 1:nrow(df_loop)) print(df_loop[i,])
## looping in R is often eschewed for 'vectorized' functions
## these are just loops written in a faster language (e.g. C)
## in R these functions are known as the 'apply family' # https://www.datacamp.com/community/tutorials/r-tutorial-apply-family
## the syntax is quite different
apply(df_loop, 2, class) # same as for(i in df_loop) print(class(i))
## for small datasets the speed gains are small
## and the advantage of for() is that it is more legible (i.e. easier to immediately see what's happening)

# we can do a lot with loops - like write a custom average function
# (I don't recommended actually doing this in an analysis)
get_mean2 <- function(data) {
  # 1. function to find N
  mylength <- function(data) {
    length = 0
    for(i in data){
      length = length + 1
    }
    return(length)
  }
  # 2. function to find the sum
  mysum <- function(data) {
    sum = 0
    for(i in data) {
      sum = sum + i
    }
    return(sum)
  }
  # 3. now you can calculate sum / n
  average = mysum(data) / mylength(data)
  return(average)
}

# test it:
## sample 100 values from a Poisson distribution with a mean of 10
x <- rpois(100,10)
get_mean2(x) # custom function
mean(x) # R function