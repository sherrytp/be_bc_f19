# author: @lrdegeest

# bayesian updating -------------------------------------------------------
# x|theta ~ Binomial(n,theta)
# theta ~ Beta(a,b)
## the beta distribution:
curve(dbeta(x,1,1),ylim=c(0,5),ylab="Beta(a,b)")
for(i in 2:20) curve(dbeta(x,i,i), add=T, col=i)

# conjugate posterior: theta|x ~ Beta(x+a, n-x+b)
sim_learning <- function(a=1,b=1,n=100,n_reps=100, q=1,true_p = 0.25,bias=F,learning=T,title=NULL) {
  beta_mean <- function() return((a) / (a + b))
  pr <- beta_mean()
  data <- c(pr)
  for(i in 1:n_reps) {
    x <- rbinom(1, 1, true_p)
    # random binomial curve, 
    # rbinom(1,1,0.5)
    if(bias){
      if(x==1) ps <- (1-q)*(beta_mean()) + q*pr
      else ps <- q*(beta_mean()) + (1-q)*pr
    } else {
      ps <- q*beta_mean() + (1-q)*pr
      # q is the confirmation bias weight, but can capture more dimensions - time horizon to dynamic model 
    }
    data <- append(data, ps)
    if(learning){
      if(x == 1){ # a + x
        a <- a + 1
      }
      else{ # a + (n-x)
        b <- b + 1
      }
    }
  }
  plot(data, type = "l", ylim = c(0,1), xlab = "Flip", ylab = "P(Heads)", main=title)
  abline(h=true_p, col="red")
  return(data)
}

# no learning - confirmation bias 
sim_learning(n_reps = 10000, learning = F)

# learning
par(mfrow=c(2,2))
sim_learning(n_reps=10, title="N=10")
sim_learning(n_reps=10^2, title="N=10^2")
sim_learning(n_reps=10^3, title="N=10^3")
sim_learning(n_reps=10^4, title="N=10^4")
dev.off()

# confirmation bias (sort of...)
sim_learning(n_reps = 1000, learning = T, bias = T, q=1)
sim_learning(n_reps = 1000, learning = T, q=1)
sim_learning(n_reps = 1000, learning = T, q=0.25)

# attempt at a model of confirmation bias
## sort of works - but needs improvement
sim_bias <- function(n=100,true_p = 0.25, samp_size=4, alpha = 0.05, bias=F,title=NULL) {
  a <- b <- 1
  beta_mean <- function() return((a) / (a + b))
  pr <- beta_mean()
  posteriors <- c(pr)
  data <- c()
  S <- c()
  for(i in 1:n) {
    x <- rbinom(samp_size, 1, true_p)
    if(bias){
      test <- binom.test(sum(x),length(x),pr)
      if(test$p.value >= alpha) {
        a <- a + sum(x) # a + x
        b <- b + (length(x) - sum(x)) # b + (n-x)
      } else {
        s <- 0
        new_pvalue <- 0
        while(new_pvalue<alpha){
          new_x <- rbinom(samp_size, 1, true_p)
          new_test <- binom.test(sum(new_x), length(new_x), pr)
          new_pvalue <- new_test$p.value
          s <- s+1
        }
        S <- append(S, s)
        a <- a + sum(new_x) # a + x
        b <- b + (length(new_x) - sum(new_x)) # b + (n-x)
      }
      ps <- beta_mean()
    } else {
      a <- a + sum(x) # a + x
      b <- b + (length(x) - sum(x)) # b + (n-x)
      ps <- beta_mean()
    }
    posteriors <- append(posteriors, ps)
  }
  par(mfrow=c(1,2))
  plot(posteriors, type = "l", col="black", ylim = c(0,1), xlab = "Sample", ylab = "P(Fair)", main=title)
  abline(h=true_p, col="red")
  hist(S,col="gray", main="", xlab="Resampling")
}
sim_bias(bias=T, samp_size = 52)
sim_bias(bias = T, samp_size = 40)

# law of large numbers ----------------------------------------------------
roll <- function(n=100) {
  possible_outcomes <- seq(1, 6, by = 1)
  outcomes <- replicate(n, sample(1:6, size=1,replace = T))
  rel_freq <- table(freq)/length(freq)
  freq <- vector()
  for(i in possible_outcomes) freq <- append(freq, sum(outcomes == i))
  df <- data.frame(possible_outcomes, freq)
  df$rel_freq <- df$freq/sum(df$freq)
  barplot(df$rel_freq, names.arg = df$possible_outcomes, 
          col="blue", xlab = 'Outcome',ylab = 'Relative Frequency', 
          main=paste0("Rolls = ",n), ylim=c(0,1))
  abline(h=1/6, col="red")
}

par(mfrow=c(2,2))
roll(10^0) # roll 1 time
roll(10^1) # roll 10 times
roll(10^2) # roll 100 times
roll(10^3) # roll 1000 times
dev.off()



## Learning felm()
oldopts <- options(lfe.threads=1)
## create covariates
x <- rnorm(1000)
x2 <- rnorm(length(x))

## individual and firm
id <- factor(sample(20,length(x),replace=TRUE))
firm <- factor(sample(13,length(x),replace=TRUE))

## effects for them
id.eff <- rnorm(nlevels(id))
firm.eff <- rnorm(nlevels(firm))

## left hand side
u <- rnorm(length(x))
y <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + u

## estimate and print result
est <- felm(y ~ x+x2| id + firm)
summary(est)
## Not run: compare with lm
summary(lm(y ~ x + x2 + id + firm-1))