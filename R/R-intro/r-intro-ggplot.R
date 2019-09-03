# author: @lrdegeest

# https://www.tidyverse.org/
# https://ggplot2.tidyverse.org/
library(tidyverse)

# quick plots -------------------------------------------------------------

# tidyverse data
data("mpg")
df <- mpg

# basic commands to plot data (qplot stands for "quick plot")
qplot(cty, data = df) # histogram
qplot(cty, hwy, data = df) # scatter plot
qplot(cty, hwy, data = df, facets = ~year) # scatter plot with one-way faceting
qplot(cty, hwy, data = df, facets = year~class) # scatter plot with two-way faceting

# simulate data -----------------------------------------------------------
treatment <- rep(0:1, each=100)
error <- rnorm(100, mean = 0, sd = 2)
gender <- rbinom(100, 1, 0.5)
y <- 2.0 + 6.0*treatment + 0.5*gender + error
df <- data.frame(y,treatment,gender)
df$treatment_string <- ifelse(df$treatment == 0, "Control", "Treatment")
df$gender_string <- ifelse(df$gender == 0, "Male", "Female")

# visualize: distributions ------------------------------------------------

## histogram: geom_histogram()
df %>% group_by(treatment_string) %>% 
  ggplot(., aes(y, fill=treatment_string)) + 
  geom_histogram(position = "dodge") +
  labs(y="Frequency", x = "Y", title="Distribution of outcomes")
  
## smooth density: geom_density()
df %>% group_by(treatment_string) %>% 
  ggplot(., aes(y, fill=treatment_string)) + 
  geom_density(alpha=0.75) +
  labs(y="Density", x = "Y", title="Distribution of outcomes") + 
  theme(legend.title=element_blank())

## smooth density by gender: facet_wrap()
df %>% group_by(treatment_string) %>% 
  ggplot(., aes(y, fill=treatment_string)) + 
  geom_density(alpha=0.75) +
  facet_wrap(~gender_string) + 
  labs(y="Density", x = "Y", title="Distribution of outcomes") + 
  theme(legend.title=element_blank())

## smooth density only female: filter()
df %>% filter(gender == 1) %>%  
  group_by(treatment_string) %>% 
  ggplot(., aes(y, fill=treatment_string)) + 
  geom_density(alpha=0.75) +
  labs(y="Density", x = "Y", title="Distribution of outcomes (female)") + 
  theme(legend.title=element_blank())

# visualize: boxplots -----------------------------------------------------
df %>% group_by(treatment_string) %>% 
  ggplot(., aes(treatment_string,y, fill=treatment_string)) + 
  geom_boxplot(alpha=0.75) +
  labs(y="Y", x = "", title="Boxplot of outcomes") + 
  theme(legend.title=element_blank())

# visualize: averages -----------------------------------------------------
## no errorbars
df %>% 
  group_by(treatment_string) %>% 
  summarize(mean = mean(y)) %>% 
  ggplot(., aes(treatment_string, mean, fill=treatment_string)) + 
  geom_bar(stat="identity") + 
  labs(y="Average Y", x="", title="Average treatment effect") + 
  theme_classic() +
  theme(legend.title=element_blank())

## with errorbars
df %>% 
  group_by(treatment_string) %>% 
  summarize(mean = mean(y), sd = sd(y), N = n()) %>% 
  mutate(se = sd / sqrt(N),
         lower = mean - qt(1 - (0.05 / 2), N - 1) * se, # qt() is the inverse CDF of the t distribution which varies depending on the DF
         upper = mean + qt(1 - (0.05 / 2), N - 1) * se) %>%  # here it gives you the t-stat that is the 97.5th percentile of the distribution
  ggplot(., aes(treatment_string, mean, fill=treatment_string)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.10) + 
  labs(y="Average Y", x="", title="Average treatment effect", subtitle = "95% confidence intervals") + 
  theme_classic() +
  theme(legend.title=element_blank())