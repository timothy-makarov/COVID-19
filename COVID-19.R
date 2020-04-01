#
# From Russia with COVID-19
# https://gist.github.com/timothy-makarov/c5a8d382aaa646fea47d4e0beb354e91
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
    840,   # https://t.me/c/1433624845/1550
    1036,  # https://t.me/COVID2019_official/113
    1264,  # https://t.me/COVID2019_official/121
    1534,  # https://t.me/COVID2019_official/128
    1836,  # https://t.me/COVID2019_official/138
    2337,  # https://t.me/COVID2019_official/153
    2777   # https://t.me/COVID2019_official/160
  )
)

df$day0 <- seq(0, length(df$infected) - 1)
df$date <- as.Date('2020-03-01') + df$day0
df$growth <- c(1, diff(df$infected))
df$infected_log <- log2(df$infected)

df <- df %>%
  filter(infected > 10)

lm1 <- lm(formula = infected_log ~ day0, data = df)
k <- lm1$coefficients[[2]]
b <- lm1$coefficients[[1]]

df$deviations <- df$day0 * k + b - df$infected_log
t.test(df$deviations)

frcst_date <- as.Date('2020-04-01')
dsm01 <- as.numeric(frcst_date - as.Date('2020-03-01')) + 1
frcst_infected <- floor(2 ^ (k * dsm01 + b))
frcst_infected

md <- data.frame(
  day0 = seq(0, 180)
)
md$date <- md$day0 + as.Date('2020-03-01')

md <- md %>%
  filter(day0 >= first(df$day0))

ggplot(df, aes(date, infected, colour = growth)) +
  geom_line() +
  geom_point() +
  ggtitle('Total number of infected') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(date, infected_log, colour = growth)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) +
  ggtitle('Total number of infected in a log scale') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(date, deviations, colour = growth)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm, formula = y ~ x) +
  ggtitle('Deviations of infected near the trend line in a log scale') +
  theme(plot.title = element_text(hjust = 0.5))
