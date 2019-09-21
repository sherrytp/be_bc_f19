# author: @lrdegeest

library(tidyverse) # install.packages("tidyverse") 
library(cowplot) # install.packages("cowplot") # to arrange multiple plots into one plot
library(latex2exp) # install.packages("latex2exp") to put math symbols in plot titles

# hypothesis testing ------------------------------------------------------

# draw 100 observations from a normal dist with mean=10 and sd=2 - DRAW Random Variables 
x <- rnorm(100,10,2)
summary(x)
hist(x)

# hypothesis test mu \neq 10 (\neq is LaTeX for "not equal to")
## calculate t-stat:
n <- length(x)
H0 <- 10
t <- (mean(x) - H0) / (sd(x) / sqrt(n))
print(t)
## calculate two-tailed p-value
p_value <- 2*pt(-abs(t), df=n-1) # all t-distributions are defined by degrees of freedom (df)
print(p_value)
## can also integrate the t-distribution:
curve(dt(x,n-1), from = -5, to = 5) ## visualize the t-distribution for our degrees of freedom; dt means density of observation 
abline(v=t, col="blue") # t-stat (positive)
abline(v=-t, col="blue") # t-stat (negative)
2*integrate(function(x) dt(x,n-1),lower = t, upper = Inf)$value # integrate from t-stat to infinity (then times 2 since distribution is symmetric)
integrate(function(x) dt(x,n-1),lower = -Inf, upper = Inf)$value # sanity check: total area = 1
integrate(function(x) dt(x, n-1), lower = -t, upper = t)$value
## replicate using the canned t-test
t.test(x, mu=10)

# CLT ---------------------------------------------------------------------

# sample from a poisson distribution with rate parameter 3
par(mfrow=c(2,2)) # create a 2x2 plot grid
hist(rpois(100,3),col="blue", xlab = "X", main = "Sample A")
hist(rpois(100,3),col="blue", xlab = "X", main = "Sample B")
hist(rpois(100,3),col="blue", xlab = "X", main = "Sample C")
hist(rpois(100,3),col="blue", xlab = "X", main = "Sample D")
dev.off() # turn off the grid

# increase the number of samples and you get a normal *sampling* distribution
par(mfrow=c(2,2)) # create a new 2x2 plot grid
hist(replicate(10^1, mean(rpois(100,3))), col="orange", xlab = "mu", main = "10 Samples")
hist(replicate(10^2, mean(rpois(100,3))), col="orange", xlab = "mu", main = "100 Samples")
hist(replicate(10^3, mean(rpois(100,3))), col="orange", xlab = "mu", main = "1000 Samples")
hist(replicate(10^4, mean(rpois(100,3))), col="orange", xlab = "mu", main = "10000 Samples")
dev.off() # turn off the grid

# CLT implies diminishing returns to sampling
true_mean <- 3 # true mean of the popluation (suppose we know it)
samples <- seq(1, 10000, 1)
m_hat <- mapply(function(x) mean(rpois(x,true_mean))-true_mean, samples) # note the use of mapply: this a vectorized loop
df <- data.frame(samples, m_hat)
cummean <- function(x) cumsum(x) / seq_along(x)
df$cum_m_hat <- cummean(df$m_hat)
p1 <- ggplot(df, aes(samples, m_hat)) + geom_line(color="blue") + 
  labs(x="Sample size", y=TeX('Bias ($\\mu - \\hat{\\mu}$)'), title=TeX('Population mean $\\mu = 3$')) +
  scale_y_continuous(limits=c(-1, 1)) +
  geom_hline(yintercept=0, color="red") +
  theme_classic() +
  scale_x_log10() 
p2 <- ggplot(df, aes(samples, cum_m_hat)) + 
  geom_hline(yintercept=0, color="red", alpha=0.5) +
  geom_line(color="orange") + 
  labs(x="Sample size", y='Average cumulative bias', title=TeX('Population mean $\\mu = 3$')) +
  scale_y_continuous(limits=c(-1, 1)) +
  theme_classic() +
  scale_x_log10()
cowplot::plot_grid(p1,p2,ncol = 2, labels = "AUTO") # notice the package "cowplot"


# regression --------------------------------------------------------------

# read auto data (note: this comes from Stata)
setwd("/Users/apple/Desktop/be_bc_f19/R/R-intro")
df <- read.csv("auto.csv")

# single regression: Price(weight)
# hypothesis test: weight has an effect on price
# H0: beta = 0
# HA: beta \neq 0
m1 <- lm(price ~ weight, data = df)
# Linear Model(y ~ x, data = df)
summary(m1)

# recreate the hypothesis test by hand 
# t-stat is just beta/standard error of beta:
t <- 2.0441 / 0.3768
p <- 2*pt(-abs(t), df=nrow(df) - 2) # df: N - number of covariates in regression (including constant); pt: probability of t-stats 
print(paste0("t-value: ", t))
print(paste0("p-value: ", p))

# multiple regression: Price(weight, foreign)
## hypothesis test: weight has an effect on price, controling for the effect of car origin (foreign)
table(df$foreign)
class(df$foreign)
levels(df$foreign)
m2 <-lm(price~weight+foreign, data=df)
summary(m2)

# Q: what does it mean to "control" for a variable? 
# A: to account for the variation in prices due to origin so we can isolate the variation in prices due to weight.
# do this by hand
# 1. create a variable that calculates the average weight of a car by origin:
df <- df %>% 
  group_by(foreign) %>% 
  mutate(weight_by_origin = mean(weight))
# 2. create a variable that subtracts `weight`` from `weight_by_origin`; `weight_by_origin` eliminates the effect of origin from weight on price 
df$weight_no_origin <- df$weight - df$weight_by_origin
# 3. Now run a regression on price and our new variable:
lm(price~weight_no_origin, data=df)
# coefficient on weight_no_origin is same as coefficient on weight in model m2


# CLASS NOTE -------------------------- 
# Expected Value: 
# The population parameters are "never" reservable and called /mu. But sample is observable, so that is why the statistics can be used to evaluate the data. 
# E() gives an unbiased estimator of the mean of the population. 
# Population of variance: sigma^2;
# we cannot observe parameters but we can observe the statistics. ###!!!! 

# Ansinton property - law of large numbers 
# Hypothesis Testing: 
# Null Hypo = mu equals to 0 
# Alter Hypo = mu is not equal to 0 
# t-stats and probablity; t-stats defines the t-distribution w degree of freedom, less df, more right skewed 
# significance: if the null hypotheis, what's the prob that there is an extreme value that I observe 
# significance check: if p < alpha where alpha is the threshold set by the user, larger p, easier to reject 
# p-value + significance checking done by convention - certain rule of thumbs 
# CLT: 
# The t-test is valid for normaally distributed variables 
# transformation you can always assume for normality but SAMPLING DISTRIBUTION of any sample of population works!!! 
# Poisson process: a stochastic process, a high frequency and large size of number 

# CLT - never seizes to amuse by infinite number of data 

alpha = 0.3 
curve(x^alpha, from = 0, to = 10)
curve((-x)^alpha, from = 0, to = 10)
