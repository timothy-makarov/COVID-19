#
# SIR ODE
# https://kingaa.github.io/thid/odes/ODEs_in_R.pdf
#

library(deSolve)

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
