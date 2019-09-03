# author: @lrdegeest

library(tidyverse) # install.packages("tidyverse") 
library(cowplot) # install.packages("cowplot") # to arrange multiple plots into one plot
library(latex2exp) # install.packages("latex2exp") to put math symbols in plot titles

# hypothesis testing ------------------------------------------------------

# draw 100 observations from a normal dist with mean=10 and sd=2
x <- rnorm(100,10,2)
summary(x)
hist(x)

# hypothesis test mu \neq 10
## calculate t-stat:
n <- length(x)
H0 <- 9
t <- (mean(x) - H0) / (sd(x) / sqrt(n))
print(t)
## calculate two-tailed p-value
p_value <- 2*pt(-abs(t), df=n-1) # all t-distributions are defined by degrees of freedom (df)
print(p_value)
## can also integrate the t-distribution:
curve(dt(x,n-1), from = -5, to = 5)
abline(v=t, col="blue")
abline(v=-t, col="blue")
2*integrate(function(x) dt(x,n-1),lower = t, upper = Inf)$value # integrate from t-stat to infinity (then times 2 since distribution is symmetric)
integrate(function(x) dt(x,n-1),lower = -Inf, upper = Inf)$value # sanity check: total area = 1
## replicate using the canned t-test
t.test(x, mu=9)

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
df <- read.csv("auto.csv")

# single regression: Price(weight)
# hypothesis test: weight has an effect on price
# H0: beta = 0
# HA: beta \neq 0
m1 <- lm(price~weight, data=df)
summary(m1)

# recreate the hypothesis test by hand 
# t-stat is just beta/standard error of beta:
t <- 2.0441 / 0.3768
p <- 2*pt(-abs(t), df=nrow(df) - 2) # df: N - number of covariates in regression (including constant)
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
# 2. create a variable that subtracts `weight`` from `weight_by_origin`
df$weight_no_origin <- df$weight - df$weight_by_origin
# 3. Now run a regression on price and our new variable:
lm(price~weight_no_origin, data=df)
# coefficient on weight_no_origin is same as coefficient on weight in model m2