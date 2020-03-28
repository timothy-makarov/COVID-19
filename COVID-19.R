#
# From Russia with COVID-19
# https://gist.github.com/timothy-makarov/c5a8d382aaa646fea47d4e0beb354e91
#

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
    1264   # https://t.me/COVID2019_official/121
  )
)

df$days_since_mar01 <- seq(1, length(df$infected))
df$inc <- c(0, diff(df$infected))
df$infected_log <- log2(df$infected)

ggplot(df, aes(days_since_mar01, infected_log, colour = inc)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) +
  ggtitle('Total number of infected in a log scale') +
  theme(plot.title = element_text(hjust = 0.5))

lm1 <- lm(formula = infected_log ~ days_since_mar01, data = df)
k <- lm1$coefficients[[2]]
b <- lm1$coefficients[[1]]
paste0('2^(', k, '*t + ', b, ')')

df$no_trend <- df$days_since_mar01 * k + b - df$infected_log
t.test(df$no_trend)

ggplot(df, aes(days_since_mar01, no_trend, colour = inc)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm, formula = y ~ x) +
  ggtitle('Deviations of infected near the trend line in a log scale') +
  theme(plot.title = element_text(hjust = 0.5))

frcst_date <- as.Date('2020-03-29')
dsm01 <- as.numeric(frcst_date - as.Date('2020-03-01')) + 1
frcst_infected <- floor(2 ^ (k * dsm01 + b))
frcst_infected

pop <- 1E+4
pop_log <- log2(pop)
pop_dsm01 <- (pop_log - b) / k
pop_dsm01_cal <- as.Date('2020-03-01') + pop_dsm01
pop_dsm01_cal
