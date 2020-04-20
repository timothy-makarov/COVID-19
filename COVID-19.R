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
df0$infected_growth_log2 <- log2(df0$infected_growth)
df0$infected_log2 <- log2(df0$infected)

df0$removed <- df0$recovered + df0$deceased
df0$removed_log2 <- log2(df0$removed)

df0$recovered_growth <- c(0, diff(df0$recovered))
df0$recovered_growth_log2 <- log2(df0$recovered_growth)
df0$recovered_log2 <- log2(df0$recovered)

df0$deceased_log2 <- log2(df0$deceased)

# Filtering
df0 <- df0 %>% filter(date > '2020-03-31')

# Analysis
infected_l2lm0 <- lm(formula = infected_log2 ~ date, df0)
df0$infected_l2lm0 <- predict(infected_l2lm0, df0)
df0$infected_lm0 <- round(2 ^ df0$infected_l2lm0)

inf_growth_l2lm0 <- lm(formula = infected_growth_log2 ~ date, df0)
df0$infected_growth_l2lm0 <- predict(inf_growth_l2lm0, df0)
df0$infected_growth_lm0 <- round(2 ^ df0$infected_growth_l2lm0)

removed_l2lm0 <- lm(formula = removed_log2 ~ date, df0)
df0$removed_l2lm0 <- predict(removed_l2lm0, df0)
df0$removed_lm0 <- round(2 ^ df0$removed_l2lm0)

recovered_l2lm0 <- lm(formula = recovered_log2 ~ date, df0)
df0$recovered_l2lm0 <- predict(recovered_l2lm0, df0)
df0$recovered_lm0 <- round(2 ^ df0$recovered_l2lm0)

recovered_growth_l2lm0 <- lm(formula = recovered_growth_log2 ~ date, df0)
df0$recovered_growth_l2lm0 <- predict(recovered_growth_l2lm0, df0)
df0$recovered_growth_lm0 <- round(2 ^ df0$recovered_growth_l2lm0)

deceased_l2lm0 <- lm(formula = deceased_log2 ~ date, df0)
df0$deceased_l2lm0 <- predict(deceased_l2lm0, df0)
df0$deceased_lm0 <- round(2 ^ df0$deceased_l2lm0)

df0$infected_per_recovered <- df0$infected / df0$recovered
df0$infected_per_deceased <- df0$infected / df0$deceased
df0$infected_per_removed <- df0$infected / df0$removed

# Presentation
df0_data <- df0 %>%
  filter(!is.na(infected)) %>%
  select(date,
         infected,
         recovered,
         deceased)

df0_forecast <- df0 %>%
  filter(date >= Sys.Date()) %>%
  select(date,
         infected_lm0,
         recovered_lm0,
         deceased_lm0)
