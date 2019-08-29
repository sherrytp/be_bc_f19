# author: @lrdegeest

# simulate data -----------------------------------------------------------
n = 100
treatment <- rep(0:1, each=n)
error <- rnorm(n, mean = 0, sd = 2)
gender <- rbinom(n, 1, 0.5)
y <- 2.0 + 6.0*treatment + 0.5*gender + error
df <- data.frame(y,treatment,gender)
df$treatment_string <- ifelse(df$treatment == 0, "Control", "Treatment")
df$gender_string <- ifelse(df$gender == 0, "Male", "Female")
df <- as_tibble(df)
print(df)

# estimate average treatment effect ---------------------------------------
m1 <- lm(y~factor(treatment), data=df)
summary(m1)

#Recall that in a linear regression, the hypothesis test of each coefficient is carried out with a t-test. 
#The t-statistic is just the estimated coefficient divided by the standard error: 
tstat = coef(summary(m1))[2] / coef(summary(m1))[,2][2]
tstat
# you can also confirm the two-tailed p-value using the student t distribution pt(t-stat, degrees of freedom):
2*pt(-abs(tstat),df=n-1)

# now control for gender
m2 <- lm(y~factor(treatment)+factor(gender), data=df)
summary(m2)

# is there a heterogenous treatment effect?
m3 <- lm(y~factor(treatment)+factor(gender) + factor(treatment)*factor(gender), data=df)
summary(m3)