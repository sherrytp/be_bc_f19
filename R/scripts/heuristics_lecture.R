# author: @lrdegeest
library(tidyverse)
library(cowplot)
library(sandwich)
library(lmtest)

# utility functions -------------------------------------------------------

# example utility
example_utility <- function(x) {
  u <- (x*(500-5*x))
  return(u)
}
# visualize
curve(example_utility, 0, 100)
# optimize: use "Brent" so we can set bounds
## why won't it optimize?
optim(1, example_utility, lower=0, upper=100, method = "Brent")

# set up inverse function to minimize
example_utility_inverse <- function(x) {
  u <- -(x*(500-5*x)) # only difference is the negative sign in front
  return(u)
}
# visualize
curve(example_utility_inverse, 0, 100)
# optimize
maximum_utility <- optim(1, inverse_revenue, lower=0, upper=100, method = "Brent")
print(maximum_utility$par) # this is the x that maximizes utility
# confirm
example_utility(maximum_utility$par) == abs(maximum_utility$value)


# recreate utility function from text
utility <- function(x,p,tq,hq) {
  u <- 20*sqrt(x-p) + 2*tq + hq
  return(u)
}
curve(utility(x,tq=0,hq=0), from =0, to = 1000)
utility(94,3,3)

utility <- function(x,p,tq,hq) {
  u <- -(20*sqrt(x-p) + 2*tq + hq)
  return(u)
}
curve(inverse_utility(x), from =0, to = 100)
optim(1, inverse_utility, lower=0, upper=100, method = "Brent")


# Gabaix et al (2006): Comparing algorithms -------------------------------

GW <- function(p,V,c=1) {
  return((p*V - c)/p)
}

DC <- function(p,V,S,c=1) {
  return(p*(V-S) - c)
}


# Apicella et al. (2014) --------------------------------------------------

# load data (assumes you are in the class directory)
df <- read.csv("be_bc_f19/data/endowment_data.csv")
df$lighter <- factor(df$lighter)
df$magnola_region <- factor(df$magnola_region)
levels(df$magnola_region) <- c("LE", "HE")

# Figure 2
## 2a
fig2a <- df %>% 
  group_by(magnola_region) %>% 
  ggplot(., aes(magnola_region, trade)) + 
  stat_summary(fun.y = mean, geom = "bar", fill="tomato") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.1) + 
  labs(y="Average trade", x="") + 
  ylim(0,1) +
  theme_classic()

## 2b
fig2b <- df %>% 
  group_by(campname) %>% 
  filter(campname != "Mizeu") %>% 
  summarize(sum_trade = sum(trade),mean_trade = mean(trade), distance_to_mangola = unique(distance_to_mangola)) %>% 
  ggplot(., aes(distance_to_mangola,mean_trade,size=sum_trade,label=campname)) + 
  geom_point(alpha=0.75,color="tomato") + 
  geom_text(aes(label=campname),hjust=0, vjust=0,size=3) + 
  labs(size = "Trades") + 
  ylim(0,1) + 
  theme_classic()

## combine the plots using the cowplot package
cowplot::plot_grid(fig2a, fig2b, labels="auto")

# t-test
t.test(trade~magnola_region, data=df, var.equal=F)

# binomial test
## Null: people trade or don't trade with equal probability
sum_tab <- table(df$magnola_region,df$trade)
d <-c(sum_tab[1], sum_tab[3])
binom.test(d, n = sum(d), p = 1/2)
binom.test(d, n = sum(d), p = 1/2, alternative = "greater")

# Table 1: linear regression
m1 <- lm(trade~magnola_region + distance_to_mangola + lighter + lighter*distance_to_mangola, data=df)
summary(m1) # are these the correct standard errors?

# cluster robust standard errors: adjust for correlated errors *within* camps (still assumes independence *between* camps)
vcov_campname <- sandwich::vcovCL(m2,cluster = df$campname)
lmtest::coeftest(m2, vcov_campname)

# addendum: islinear regression the best model here? 
## since "trade" is a binary outcome, it's average is the probability of a trade
## the estimated coefficients are the marginal effects on this probability
## problem: a linear equation has no bounds. it continues up to positive infinity and down to negative infinity
hist(predict(m1, type="response")) # here we are lucky - no predicted probabilities outside zero or one. but it is possible.

## alternative: probit or logit. Instead of y = xb + e, we have y = F(xb + e) where F() is a "link function". more on this later.
m2 <- glm(trade~magnola_region + distance_to_mangola + lighter + lighter*distance_to_mangola, data=df, binomial(link = "probit"))
hist(predict(m2, type="response")) # looks very similar to before