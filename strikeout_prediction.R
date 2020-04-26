# Using linear regression to predict 2nd-half strikeout percentages from available first-half data

# Libraries 
library(tidyverse)
library(mosaic)
library(ggplot2)
library(psych)
library(SDSRegressionR)

# Data
strikeouts <- read_csv("strikeouts.csv")
k_per <- strikeouts$`K%`


# The data describes first-half pitching statistics from the 2017 MLB season. The columns "2ndHalfK%" and "2ndHalfIP" are the only columns 
# containing information from the second half of the season. The "2ndHalfK%" was used only to check the accuracy of the predictions. 


# Correlation Matrix - which variables should be included in the model?
favstats(~ k_per)

strikeouts %>% 
  select(`K%`, ERA, FIP, xFIP, AVG, `BB%`, `Swing%`, `Contact%`, `LD%`, `GB%`, `FB%`) %>%  
  cor(use="pairwise.complete.obs")

r_test <- strikeouts %>% 
  select(`K%`, ERA, FIP, xFIP, AVG, `BB%`, `Swing%`, `Contact%`, `LD%`, `GB%`, `FB%`) %>% 
  corr.test()

r_test$r

# Strongest correlation: xFIP, Contact%, FIP, AVG, ERA

# Make some plots - Visualization is important
strikeouts$con_per <- strikeouts$`Contact%`
strikeouts$k_per <- strikeouts$`K%`

gf_point(k_per ~ xFIP, data = strikeouts, size = 3, alpha = 0.8) %>% 
  gf_theme(theme_bw) %>% 
  gf_lm()

gf_point(k_per ~ con_per, data = strikeouts, alpha= 0.8, size = 3) %>% 
  gf_theme(theme_bw) %>% 
  gf_lm()



# Initial model
initial_model <- lm(k_per ~ xFIP + con_per + AVG + FIP + ERA, data = strikeouts)
summary(initial_model)


# Diagnostics
library(car)
vif(initial_model) # Looks good
residFitted(initial_model) # Looks good

# Cook's Distance to find outliers
c <- cooksPlot(initial_model, key.variable = "Name", print.obs = T, save.cutoff = T)
c
2*cooksCutOff

# Remove outliers
g_strike <- strikeouts %>% 
  filter(Name %not_in% c(c$Name[1:4]))

# Re-run the model
m2 <- lm(k_per ~ xFIP + FIP + AVG + ERA + con_per, data = g_strike)
summary(m2)

# Improvement! 

# Make predictions
library(car)
linearHypothesis(m2, "xFIP = 3")

g_strike$fitted_k <- m2$fitted.values # The model's predictions of k_per

# Does adding IP as an input change things?

final_model <- lm(k_per ~ xFIP + FIP + AVG + ERA + con_per + IP, data = g_strike)
summary(final_model)
# This model has an r-squared of 0.8517 and an F-statistic of 272.7 (6,285), p < 0.05; it's a good model for predicting first-half 
#strikeout percentages.


# IP isn't a significant predictor, but I'll keep it in anyway to incorporate the given second-half data.


# Predicting based on "new" data. 

con_per <- g_strike$`Contact%`

second_half_df <- data.frame(IP = g_strike$`2ndHalfIP`,
                             xFIP = g_strike$xFIP,
                             FIP = g_strike$FIP, 
                             AVG = g_strike$AVG,
                             ERA = g_strike$ERA,
                             con = g_strike$con_per)


# Second half predictions
g_strike$second_half_predict <- predict(object = final_model, newdata = second_half_df)


# With prediction intervals
second_predictions <- predict(object = final_model, newdata = second_half_df, interval = "prediction")
second_predictions


# Evaluate: Compute the root mean squared error
library(Metrics)
rmse(g_strike$`2ndHalfK%`, second_predictions)

# A root mean squared error of 0.067 is pretty solid given the range of the data.

# Test on just fitted values
rmse(g_strike$`2ndHalfK%`, g_strike$fitted_k)

# an even smaller rmse of 0.054 for the fitted values from the model, which don't take into account the 2nd half IP.


g_strike <- cbind(g_strike, second_predictions)

# More plots for visualization - predicted vs actual values
plot <- ggplot(g_strike, aes(second_half_predict, `2ndHalfK%`))+
  geom_point()+
  stat_smooth(method = lm)

plot

fin <- lm(`2ndHalfK%` ~ second_half_predict, data = g_strike)
summary(fin)


# Summary: 
# The predicted 2nd half strikeout percentage values were able to decently predict the actual 2nd half strikeout percentage values. The predictions
# had a root mean squared error of 0.067, and they were able to explain 38 percent of the variance in the relationship between the two. Predicting
# second half statistics is always a tricky endeavor, as many players drastically improve in the second half or falter down the stretch for a 
# variety of reasons, so I'd say the performance of this model is fairly adequate given the task.  
