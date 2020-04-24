#
# From Russia with COVID-19
# https://github.com/timothy-makarov/COVID-19
#

library(dplyr)
library(ggplot2)

rm(list = ls())

# Environment
setwd("~/Git/COVID-19")

# Import
df0 <- read.csv('COVID-19.csv')
df0$date <- as.Date(df0$date)

df0$infected_growth <- c(1, diff(df0$infected))
df0$infected_l2 <- log2(df0$infected)
df0$infected_rate <- round((df0$infected / lag(df0$infected) - 1) * 100, 2)

df0$recovered_growth <- c(0, diff(df0$recovered))
df0$recovered_l2 <- log2(df0$recovered)

df0$deceased_growth <- c(0, diff(df0$deceased))
df0$deceased_l2 <- log2(df0$deceased)

df0$removed <- df0$recovered + df0$deceased

# Filtering
df0 <- df0 %>% filter(date > '2020-03-31')

# Analysis
infected_l2lm0 <- lm(formula = infected_l2 ~ date, df0)
df0$infected_l2lm0 <- predict(infected_l2lm0, df0)
df0$infected_lm0 <- round(2 ^ df0$infected_l2lm0)
df0$infected_lm0_err <- round((df0$infected_lm0 / df0$infected - 1) * 100, 2)

inf_growth_lm0 <- lm(formula = infected_growth ~ date, df0)
df0$infected_growth_lm0 <- round(predict(inf_growth_lm0, df0))

infected_rate_lm0 <- lm(formula = infected_rate ~ date, df0)
df0$infected_rate_lm0 <- predict(infected_rate_lm0, df0)

recovered_l2lm0 <- lm(formula = recovered_l2 ~ date, df0)
df0$recovered_l2lm0 <- predict(recovered_l2lm0, df0)
df0$recovered_lm0 <- round(2 ^ df0$recovered_l2lm0)
df0$recovered_lm0_err <- round((df0$recovered_lm0 / df0$recovered - 1) * 100, 2)

recovered_growth_lm0 <- lm(formula = recovered_growth ~ date, df0)
df0$recovered_growth_lm0 <- round(predict(recovered_growth_lm0, df0))

deceased_l2lm0 <- lm(formula = deceased_l2 ~ date, df0)
df0$deceased_l2lm0 <- predict(deceased_l2lm0, df0)
df0$deceased_lm0 <- round(2 ^ df0$deceased_l2lm0)
df0$deceased_lm0_err <- round((df0$deceased_lm0 / df0$deceased - 1) * 100, 2)

deceased_growth_lm0 <- lm(formula = deceased_growth ~ date, df0)
df0$deceased_growth_lm0 <- round(predict(deceased_growth_lm0, df0))

# https://www.worldometers.info/coronavirus/coronavirus-death-rate/
df0$cfr <- round(df0$deceased / lag(df0$infected, 14) * 100, 2)

# Presentation
df0_data <- df0 %>%
  filter(!is.na(infected)) %>%
  select(date,
         infected,
         infected_growth,
         recovered,
         recovered_growth,
         deceased,
         deceased_growth)

df0_model <- df0 %>%
  select(date,
         infected_lm0,
         infected_lm0_err,
         recovered_lm0,
         recovered_lm0_err,
         deceased_lm0,
         deceased_lm0_err)
