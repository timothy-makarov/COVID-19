#
# From Russia with COVID-19
# https://gist.github.com/timothy-makarov/c5a8d382aaa646fea47d4e0beb354e91
#

library(deSolve)
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

df <- df %>%
  select(date, day0, growth, infected, infected_log2)

df <- df %>%
  filter(infected > 10)

lm1 <- lm(formula = infected_log2 ~ day0, data = df)
k <- lm1$coefficients[[2]]
b <- lm1$coefficients[[1]]

df$lm_log2 <- df$day0 * k + b
df$error_log2 <- df$lm_log2 - df$infected_log2
t.test(df$error_log2)

df$lm <- round(2 ^ (df$day0 * k + b))
df$error <- df$lm - df$infected

ggplot(df, aes(date, infected, colour = growth)) +
  geom_line(data = df, aes(date, lm), color = 'red', size = 0.25, linetype = 'dashed') +
  geom_point(data = df, aes(date, lm), color = 'red', size = 0.5) +
  geom_line() +
  geom_point()

ggplot(df, aes(date, infected_log2, colour = growth)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

ggplot(df, aes(date, error, colour = growth)) +
  geom_point() +
  geom_smooth(method = loess, formula = y ~ x)

ggplot(df, aes(date, error_log2, colour = growth)) +
  geom_point() +
  geom_smooth(method = loess, formula = y ~ x)

#
# -=[ ODE ]=-
# https://kingaa.github.io/thid/odes/ODEs_in_R.pdf
#

# beta - transmition rate
# gamma - recovery rate

closed.sir.model <- function(t, x, params) {
  S <- x[1]
  I <- x[2]
  R <- x[3]
  
  beta <- params["beta"]
  gamma <- params["gamma"]
  
  dSdt <- -beta * S * I
  dIdt <- beta * S * I - gamma * I
  dRdt <- gamma * I
  
  dxdt <- c(dSdt, dIdt, dRdt)
  
  list(dxdt)
}

params <- c(beta = 400, gamma = 365 / 13)

times <- seq(from = 0, to = 60 / 365, by = 1 / 365 / 4)
xstart <- c(S = 0.999, I = 0.001, R = 0.000)

out <- as.data.frame(ode(
  func = closed.sir.model,
  y = xstart,
  times = times,
  parms = params
))


plot(S ~ time, data = out, type = 'l', col = 'blue')
lines(I ~ time, data = out, col = 'brown')
lines(R ~ time, data = out, col = 'green')

paste0('R_0 = ', round(params['beta'] / params['gamma'], 2))
