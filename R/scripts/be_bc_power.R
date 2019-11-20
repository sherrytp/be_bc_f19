# author: @lrdgeest

library(tidyselect)
library(MESS)

# one-sample --------------------------------------------------------------
# similar to Stata: power onemean 10 12, sd(5) p(0.8) alpha(0.05) onesided
power.t.test(power = .80, delta = 2, sd=5, type = "one.sample", alternative = "one.sided")

# comparative statics
## what happens when you increase the variance for a fixed effect size d?
par(mfrow=c(1,3))
s <- 1:20
N <- vector()
for(i in s) N <- append(N, power.t.test(power = .80, delta = 2, sd=i, type = "one.sample", alternative = "one.sided")[1])
plot(s,N, col="blue", pch=16, xlab="Standard deviation", ylab="N")
lines(s,N, col="blue")
# or if you increase the effect size for a fixed s?
d <- seq(1,5,0.25)
N <- vector()
for(i in d) N <- append(N, power.t.test(power = .80, delta = i, sd=5, type = "one.sample", alternative = "one.sided")[1])
plot(d,N, col="orange", pch=16, xlab="Effect size (mu_1 - mu_0)", ylab="N")
lines(d,N, col="orange")
# or if you vary the power for fixed s and d?
beta <- seq(0.1,0.95,0.05)
N <- vector()
for(i in beta) N <- append(N, power.t.test(power = i, delta = 2, sd=5, type = "one.sample", alternative = "one.sided")[1])
plot(beta,N, col="red", pch=16, xlab="Power (1 - beta)", ylab="N")
lines(beta,N, col="red")
dev.off()

# two samples -------------------------------------------------------------
## assuming equal variances
## same as Stata: power twomeans 10 12, sd1(5) sd2(5) p(0.8) onesided 
power.t.test(power = .80, delta = 2, sd = 5, type = "two.sample", alternative = "one.sided")
## assuming unequal variances
## same as Stata: power twomeans 10 12, sd1(4) sd2(7.84) p(0.8) onesided 
## need MESS package: https://github.com/ekstroem/MESS
MESS::power_t_test(n=NULL, sd=4, power=.8, ratio=1, sd.ratio=7.84/4, delta=2, alternative = "one.sided")
## vary the relative standard deviations and see what happens
sdC <- 4
ratio <- seq(1,10,1)/sdC
N <- vector()
for(i in ratio) { 
  n <- MESS::power_t_test(n=NULL, sd=sdC, power=.8, ratio=1, sd.ratio=i, delta=2, alternative = "one.sided")[[1]][1]
  N <- append(N,n)
}
plot(ratio,N, col="blue", pch=16, xlab="sdT/sdC", ylab="N")
lines(ratio,N, col="blue")

# is my test sufficiently powered? 
df <- data.frame("d" = seq(1,5,0.25))
df$pwr50 <- sapply(d, function(i) MESS::power_t_test(n=50, sd=6, power=NULL, ratio=1, sd.ratio=1, delta=i, alternative = "one.sided")$power)
df$pwr100 <- sapply(d, function(i) MESS::power_t_test(n=100, sd=6, power=NULL, ratio=1, sd.ratio=1, delta=i, alternative = "one.sided")$power)
df$pwr200 <- sapply(d, function(i) MESS::power_t_test(n=200, sd=6, power=NULL, ratio=1, sd.ratio=1, delta=i, alternative = "one.sided")$power)
df %>% 
  gather(key="N", value="power", c(pwr50, pwr100, pwr200)) %>% 
  mutate(N = factor(gsub("pwr", "", N))) %>% 
  ggplot(., aes(d, power, color=N)) + 
  geom_hline(yintercept = 0.8, linetype = "dashed") + 
  geom_line() + geom_point() + 
  labs(x = "Absolute effect size", y = "Power", title = "Equal variances (SD = 6)") + 
  theme_classic()

# unequal groups
## suppose we have high-incentive treatment and low-incentive control
## suppose c_T = 4c_C
## then we expect n_T = 2c_C (so 2 control subjects for every one treatment subject)
## you have to rearrange arguments in power_t_test to make this work
## the argument "ratio" is n2/n1, where n2 is LARGER goup
## problem is "ratio" only takes values of 1 or higher
## so set "ratio = 2" and then rearrange the sd and sd.ratio so that the treatment is the reference point
### i.e. set "sd" to the treatment's sd (sd_T) and "sd.ratio" to sd_C/sd_T
MESS::power_t_test(n=NULL, sd=7.84, power=.8, ratio=2, sd.ratio=4/7.84, delta=2, alternative = "one.sided")
## same as Stata: power twomeans 10 12, sd1(4) sd2(7.84) p(0.8) nratio(0.5) onesided  



# multiple hypothesis testing ---------------------------------------------
## visual
curve(1 - (1 - 0.05)^x, 1, 100, xlab = "Number of Tests (m)", ylab = "Pr(At Least One Type I Error)", col="blue")

# simulation based on a great example by https://www.stat.berkeley.edu/~mgoldman/Section0402.pdf
# generate 900 x ~ N(0,1) and 100 x ~ N(3,1)
# H0: x = 0; HA: x  0
set.seed(4321)
n <- 1000
alpha <- 0.05
x <- c(rnorm(n-100, mean = 0, sd = 1), rnorm(n-900, mean = 3, sd = 1)) 
p <- pnorm(x, lower.tail = F) # calculate p-values


show_error_rate <- function(results,error=1){
  if(error==1) print(table(results[1:900])/length(results[1:900]))
  else if(error==2) print(table(results[901:1000])/length(results[901:1000]))
  else if(error !=1 | error != 2) print("error must be 1 or 2")
}


# no adjustments
test_outcomes <- p > alpha # our n hypothesis tests
# first 900 observations x ~ N(0,1)
show_error_rate(test_outcomes, error=1) # Type 1 error (false positives, the FALSE column) 
# last 100 observations x ~ N(3,1)
show_error_rate(test_outcomes, error=2) # Type II error (false negatives, the TRUE column)
## Type 1 error is ~0.05 (close to alpha)
## Type 2 error is ~0.09

# bonferroni correction
test_outcomes_bf <- p > (alpha/n)
show_error_rate(test_outcomes_bf, error=1)
show_error_rate(test_outcomes_bf, error=2)
# type 1 goes down to 0.001
# but type 1 goes up to 0.85

# FDR (false discovery rate)
## 1. order the p-values smallest to largest
## 2. check if the kth p-value is greater than alpha/n
psort <- sort(p)
test_outcomes_fdr <- vector()
for (i in 1:n) test_outcomes_fdr <- c(test_outcomes_fdr, p[i] > match(p[i],psort) * alpha/n)
show_error_rate(test_outcomes_fdr, error=1)
show_error_rate(test_outcomes_fdr, error=2)
# type 1 still low (like bonferonni)
# type 2 lower than bonferonni