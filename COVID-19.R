#
# From Russia with COVID-19
# https://github.com/timothy-makarov/COVID-19
#

library(dplyr)
library(ggplot2)
library(scales)

rm(list = ls())


#
# Functions
#

proc_period <- function(df_in, t0, t1) {
  df_out <- df_in %>% filter(date >= t0 & date <= t1)

  infected_l2lm0 <- lm(formula = infected_l2 ~ date, df_out)
  df_out$infected_l2lm0 <- predict(infected_l2lm0, df_out)
  df_out$infected_lm0 <- round(2 ^ df_out$infected_l2lm0)
  df_out$infected_lm0_err <- round((df_out$infected_lm0 / df_out$infected - 1) * 100, 2)

  infected_growth_lm0 <- lm(formula = infected_growth ~ date, df_out)
  df_out$infected_growth_lm0 <- round(predict(infected_growth_lm0, df_out))

  infected_rate_lm0 <- lm(formula = infected_rate ~ date, df_out)
  df_out$infected_rate_lm0 <- predict(infected_rate_lm0, df_out)

  recovered_l2lm0 <- lm(formula = recovered_l2 ~ date, df_out)
  df_out$recovered_l2lm0 <- predict(recovered_l2lm0, df_out)
  df_out$recovered_lm0 <- round(2 ^ df_out$recovered_l2lm0)
  df_out$recovered_lm0_err <- round((df_out$recovered_lm0 / df_out$recovered - 1) * 100, 2)

  recovered_growth_lm0 <- lm(formula = recovered_growth ~ date, df_out)
  df_out$recovered_growth_lm0 <- round(predict(recovered_growth_lm0, df_out))

  deceased_l2lm0 <- lm(formula = deceased_l2 ~ date, df_out)
  df_out$deceased_l2lm0 <- predict(deceased_l2lm0, df_out)
  df_out$deceased_lm0 <- round(2 ^ df_out$deceased_l2lm0)
  df_out$deceased_lm0_err <- round((df_out$deceased_lm0 / df_out$deceased - 1) * 100, 2)

  deceased_rate_lm0 <- lm(formula = deceased_rate ~ date, df_out)
  df_out$deceased_rate_lm0 <- predict(deceased_rate_lm0, df_out)

  recovered_rate_lm0 <- lm(formula = recovered_rate ~ date, df_out)
  df_out$recovered_rate_lm0 <- predict(recovered_rate_lm0, df_out)
  
  deceased_growth_lm0 <- lm(formula = deceased_growth ~ date, df_out)
  df_out$deceased_growth_lm0 <- round(predict(deceased_growth_lm0, df_out))

  # Presentation
  table_data <- df_out %>%
    filter(!is.na(infected)) %>%
    select(date,
           infected,
           infected_growth,
           recovered,
           recovered_growth,
           deceased,
           deceased_growth)

  table_model <- df_out %>%
    select(date,
           infected_lm0,
           infected_lm0_err,
           recovered_lm0,
           recovered_lm0_err,
           deceased_lm0,
           deceased_lm0_err)
  
  return(list(
    df = df_out,

    data = table_data,
    model = table_model,

    infected_rate_lm0 = infected_rate_lm0,
    recovered_rate_lm0 = recovered_rate_lm0,
    deceased_rate_lm0 = deceased_rate_lm0,

    recovered_growth_lm0 = recovered_growth_lm0,
    infected_growth_lm0 = infected_growth_lm0,
    deceased_growth_lm0 = deceased_growth_lm0,

    recovered_l2lm0 = recovered_l2lm0,
    deceased_l2lm0 = deceased_l2lm0
  ))
}


find_y_intersect <- function(lmod) {
  k <- lmod$coefficients[[2]]
  b <- lmod$coefficients[[1]]
  return(as.Date('1970-01-01') + (-b / k))
}


#
# Environment
#
setwd("~/Git/COVID-19")


#
# Import data
#
df0 <- read.csv('COVID-19.csv')
df0$date <- as.Date(df0$date)

# Infected
df0$infected_growth <- c(1, diff(df0$infected))
df0$infected_l2 <- log2(df0$infected)
df0$infected_rate <- round((df0$infected / lag(df0$infected) - 1) * 100, 2)

# Recovered
df0$recovered_growth <- c(0, diff(df0$recovered))
df0$recovered_l2 <- log2(df0$recovered)
df0$recovered_rate <- round((df0$recovered / lag(df0$recovered) - 1) * 100, 2)

# Deceased
df0$deceased_growth <- c(0, diff(df0$deceased))
df0$deceased_l2 <- log2(df0$deceased)
df0$deceased_rate <- round((df0$deceased / lag(df0$deceased) - 1) * 100, 2)

# Removed
df0$removed <- df0$recovered + df0$deceased

# Active cases
df0$active <- df0$infected - df0$removed

#
# EDA
#
ggplot(df0) +
  geom_col(aes(date, active))
