# From Russia with COVID-19

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
    438,    # https://t.me/map_mind/3435
    658     # https://t.me/COVID2019_official/100
  )
)

df$days_since_mar01 <- seq(1, length(df$infected))
df$inc <- c(0, diff(df$infected))
df$infected_log <- log2(df$infected)

ggplot(df, aes(days_since_mar01, infected_log, colour = inc)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

lm1 <- lm(formula = infected_log ~ days_since_mar01, data = df)
k <- lm1$coefficients[[2]]
b <- lm1$coefficients[[1]]

df$no_trend <- df$days_since_mar01 * k + b - df$infected_log
t.test(df$no_trend)

ggplot(df, aes(days_since_mar01, no_trend, colour = inc)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm, formula = y ~ x)

frcst_date <- as.Date('2020-03-24')
dsm01 <- as.numeric(frcst_date - as.Date('2020-03-01')) + 1
frcst_infected <- floor(2 ^ (k * dsm01 + b))
frcst_infected

pop <- 1E+4
pop_log <- log2(pop)
pop_dsm01 <- (pop_log - b) / k
pop_dsm01_cal <- as.Date('2020-03-01') + pop_dsm01
pop_dsm01_cal
