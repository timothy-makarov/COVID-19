#
# From Russia with COVID-19
# https://github.com/timothy-makarov/COVID-19
#

library(dplyr)
library(ggplot2)

rm(list = ls())

df0 <- data.frame(
  infected = c(
    1,
    2,
    3,
    6,
    6,
    7,
    10,
    14,
    17,
    20,
    20,
    28,
    34,
    45,
    59,
    63,
    93,
    114,
    147,
    199,
    253,
    306,
    367,
    438,   # https://t.me/map_mind/3435
    658,   # https://t.me/COVID2019_official/100
    840,   # https://t.me/COVID2019_official/107
    1036,  # https://t.me/COVID2019_official/113
    1264,  # https://t.me/COVID2019_official/121
    1534,  # https://t.me/COVID2019_official/128
    1836,  # https://t.me/COVID2019_official/138
    2337,  # https://t.me/COVID2019_official/153
    2777,  # https://t.me/COVID2019_official/160
    3548,  # https://t.me/COVID2019_official/168
    4149,  # https://t.me/COVID2019_official/179
    4731,  # https://t.me/COVID2019_official/189
    5389,  # https://t.me/COVID2019_official/203
    6343,  # https://t.me/COVID2019_official/205
    7497,  # https://t.me/COVID2019_official/212
    8672,  # https://t.me/COVID2019_official/220
    10131, # https://t.me/COVID2019_official/227
    NA,    # Forecast for 5 days
    NA,
    NA,
    NA,
    NA
  )
)

df0$day0 <- seq(0, length(df0$infected) - 1)
df0$date <- as.Date('2020-03-01') + df0$day0
df0$growth <- c(1, diff(df0$infected))
df0$infected_log2 <- log2(df0$infected)

df0 <- df0 %>% select(date, day0, growth, infected, infected_log2)
df0 <- df0 %>% filter(day0 > 14)

# Last 2 weeks (14 and +5 days forecast)
df1 <- df0 %>%
  filter(date > last(df0$date) - 19)

# Last 1 week (7 and +5 days forecast)
df2 <- df0 %>%
  filter(date > last(df0$date) - 12)

lm0 <- lm(formula = infected_log2 ~ day0, data = df0)
df0$lm_log2 <- predict(lm0, df0)
df0$error_log2 <- df0$lm_log2 - df0$infected_log2
df0$lm <- round(2 ^ df0$lm_log2)
df0$error <- df0$lm - df0$infected

lm1 <- lm(formula = infected_log2 ~ day0, data = df1)
df1$lm_log2 <- predict(lm1, df1)
df1$lm <- round(2 ^ df1$lm_log2)
df1$lm_growth <- c(0, diff(df1$lm))

lm2 <- lm(formula = infected_log2 ~ day0, data = df2)
df2$lm_log2 <- predict(lm2, df2)
df2$lm <- round(2 ^ df2$lm_log2)
df2$lm_growth <- c(0, diff(df2$lm))

# Slope decrease rate
dr <- (lm1$coefficients[[2]] - lm2$coefficients[[2]]) / 7
# Most accurate model slope coefficient
days_left <- round(lm2$coefficients[[2]] / dr)
final_day <- last(df0$date) + days_left
paste0('Epidemic ceases by ', final_day)

ggplot(df0, aes(date, infected, colour = growth)) +
  geom_line() +
  geom_point() +
  geom_line(data = df0, aes(date, lm), color = 'red', size = 0.25, linetype = 'dashed') +
  geom_point(data = df0, aes(date, lm), color = 'red', size = 0.5) +
  geom_line(data = df1, aes(date, lm), color = 'blue', size = 0.25, linetype = 'dashed') +
  geom_point(data = df1, aes(date, lm), color = 'blue', size = 0.5) +
  geom_line(data = df2, aes(date, lm), color = 'green', size = 0.25, linetype = 'dashed') +
  geom_point(data = df2, aes(date, lm), color = 'green', size = 0.5)

ggplot(df0, aes(date, infected_log2, colour = growth)) +
  geom_point() +
  geom_line(data = df0, aes(date, lm_log2), color = 'red', size = 0.25, linetype = 'dashed') +
  geom_point(data = df0, aes(date, lm_log2), color = 'red', size = 0.5) +
  geom_line(data = df1, aes(date, lm_log2), color = 'blue', size = 0.25, linetype = 'dashed') +
  geom_point(data = df1, aes(date, lm_log2), color = 'blue', size = 0.5) +
  geom_line(data = df2, aes(date, lm_log2), color = 'green', size = 0.25, linetype = 'dashed') +
  geom_point(data = df2, aes(date, lm_log2), color = 'green', size = 0.5)
