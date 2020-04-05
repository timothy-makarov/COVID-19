#
# From Russia with COVID-19
# https://github.com/timothy-makarov/COVID-19
#

library(dplyr)
library(ggplot2)

rm(list = ls())

df <- data.frame(
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
    5389   # https://t.me/COVID2019_official/203
  )
)

df$day0 <- seq(0, length(df$infected) - 1)
df$date <- as.Date('2020-03-01') + df$day0
df$growth <- c(1, diff(df$infected))
df$infected_log2 <- log2(df$infected)

df <- df %>% select(date, day0, growth, infected, infected_log2)
df <- df %>% filter(infected > 10)

lm1 <- lm(formula = infected_log2 ~ day0, data = df)

df$lm_log2 <- predict(lm1, df)
df$error_log2 <- df$lm_log2 - df$infected_log2
t.test(df$error_log2)

df$lm <- round(2^df$lm_log2)
df$error <- df$lm - df$infected

ggplot(df, aes(date, infected, colour = growth)) +
  geom_line(data = df, aes(date, lm), color = 'red', size = 0.25, linetype = 'dashed') +
  geom_point(data = df, aes(date, lm), color = 'red', size = 0.5) +
  geom_line() +
  geom_point()

ggplot(df, aes(date, infected_log2, colour = growth)) +
  geom_line(data = df, aes(date, lm_log2), color = 'red', size = 0.25, linetype = 'dashed') +
  geom_point(data = df, aes(date, lm_log2), color = 'red', size = 0.5) +
  geom_point()
