#
# From Russia with COVID-19
# https://github.com/timothy-makarov/COVID-19
#

library(dplyr)
library(ggplot2)

rm(list = ls())

# Import
df0 <- read.csv('COVID-19.csv')
df0$date <- as.Date(df0$date)
df0$infected_growth <- c(1, diff(df0$infected))
df0$infected_log2 <- log2(df0$infected)
df0$removed <- df0$recovered + df0$deceased
df0$removed_log2 <- log2(df0$removed)

# Filtering
df0 <- df0 %>% filter(date > '2020-03-31')

# Analysis
infected_l2lm0 <- lm(formula = infected_log2 ~ date, df0)
df0$infected_l2lm0 <- predict(infected_l2lm0, df0)
df0$infected_lm0 <- 2 ^ df0$infected_l2lm0

removed_l2lm0 <- lm(formula = removed_log2 ~ date, df0)
df0$removed_l2lm0 <- predict(removed_l2lm0, df0)
df0$removed_lm0 <- 2 ^ df0$removed_l2lm0

df0$infected_per_recovered <- df0$infected / df0$recovered
df0$infected_per_deceased <- df0$infected / df0$deceased
df0$recovered_per_deceased <- df0$recovered / df0$deceased

# Plotting
ggplot(df0) +
  geom_point(aes(date, infected), color = 'brown', size = 0.75) +
  geom_line(
    aes(date, infected_lm0),
    color = 'brown',
    size = 0.25,
    linetype = 'dashed'
  )

ggplot(df0) +
  geom_point(aes(date, infected_log2), size = 0.75) +
  geom_line(aes(date, infected_l2lm0),
            size = 0.25,
            linetype = 'dashed')

ggplot(df0) +
  geom_point(aes(date, removed_log2), size = 0.75) +
  geom_line(aes(date, removed_l2lm0),
            size = 0.25,
            linetype = 'dashed')

ggplot(df0) +
  geom_point(aes(date, removed), size = 0.75) +
  geom_line(aes(date, removed_lm0),
            size = 0.25,
            linetype = 'dashed')

ggplot(df0) +
  geom_point(aes(date, infected_per_recovered), size = 0.75) +
  geom_line(aes(date, infected_per_recovered),
            size = 0.25,
            linetype = 'dashed')

ggplot(df0) +
  geom_point(aes(date, infected_per_deceased), size = 0.75) +
  geom_line(aes(date, infected_per_deceased),
            size = 0.25,
            linetype = 'dashed')

ggplot(df0) +
  geom_point(aes(date, recovered_per_deceased), size = 0.75) +
  geom_line(aes(date, recovered_per_deceased),
            size = 0.25,
            linetype = 'dashed')
