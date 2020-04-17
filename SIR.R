#
# SIR Model
#
# References:
#   - https://kingaa.github.io/thid/odes/ODEs_in_R.pdf
#   - https://staff.math.su.se/hoehle/blog/2020/03/16/flatteningthecurve.html
#   - https://towardsdatascience.com/social-distancing-to-slow-the-coronavirus-768292f04296
#   - https://triplebyte.com/blog/modeling-infectious-diseases
#

library(deSolve)

rm(list = ls())

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

N <- 12e+6
Z0 <- 10
# Basic reproduction number
R0 <- 2.5
# Recovery rate
gamma0 <- 1 / 2.25
# Transmition rate
beta0 <- R0 / gamma0
params <- c(beta = beta0, gamma = gamma0)
times <- seq(from = 0, to = 30, by = 0.01)
xstart <- c(S = 1 - Z0 / N, I = Z0 / N, R = 0.0)

out <- as.data.frame(ode(
  func = closed.sir.model,
  y = xstart,
  times = times,
  parms = params
))

out$S <- round(N * out$S)
out$I <- round(N * out$I)
out$R <- round(N * out$R)
# Magic number to scale the X-axis (time), taken from observed data (official)
out$time <- round(out$time / 0.03125)

# Filter
out <- subset(out, I > 2700)
out$time <- as.Date('2020-04-01') + out$time - 35

plot(S ~ time, data = out, type = 'l', col = 'blue')
lines(I ~ time, data = out, col = 'brown')
lines(R ~ time, data = out, col = 'green')
