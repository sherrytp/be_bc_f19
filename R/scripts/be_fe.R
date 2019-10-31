# author: @lrdgeest

library(tidyverse)
library(lfe)
library(plm)

df <- read.csv("../data/Grunfeld.csv")
df$Firm <- factor(df$Firm)

# Plot time series of each variable, colored by Firm
df %>%
  gather(variable, value, -c(Year,Firm)) %>% # gather all variables except Year and Firm 
  ggplot(., aes(Year, value, color=Firm)) + 
  geom_point() + 
  facet_wrap(~variable)

# Plot I~C, colored by Firm
ggplot(df, aes(C, I, color=Firm)) + 
  geom_point() 

# Averages
df %>% 
  group_by(Firm) %>% 
  summarise(mean_I = mean(I), mean_C = mean(C), mean_F = mean(F))

# Fixed effects with plm
m_fe_plm <- plm(I~C, data = df, index = c("Firm", "Year"), model="within")
summary(m_fe_plm)
fixef(m_fe_plm) # view the fixed effects (constants for each firm)

# Fixed effects with lfe
m_fe_lfe <- felm(I~C | Firm | 0 | 0, df)
summary(m_fe_lfe)

# Fixed effects with lm
m_fe_lm <- lm(I~C+Firm+0, data=df) # drop the intercept to make comparison with fixef(m_fe_plm)
summary(m_fe_lm)

# Fixed effects with the within esimator
df %>% 
  group_by(Firm) %>% 
  mutate(I_fe = I - mean(I), C_fe = C - mean(C)) %>% 
  lm(I_fe ~ C_fe, data =.)