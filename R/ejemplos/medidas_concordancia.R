source("R/lib/copula.R")
library(latex2exp)

n.sim <- 1e4

set.seed(42)

vec.u <- runif(n.sim)
vec.v <- runif(n.sim)

kendall_tau <- function(gamma) {
  set.seed(42)
  omega <- sim_omega(1e4, gamma = gamma)
  
  c <- vector(mode = "double", length = length(vec.u))
  dc <- vector(mode = "double", length = length(vec.u))
  
  for (i in 1:length(vec.u)) {
    u <- vec.u[i]
    v <- vec.v[i]
    c[i] <- copula(u, v, omega$omega1, omega$omega2)
    dc[i] <- densidad_copula(u, v, omega$omega1, omega$omega2)
  }
  
  return(4 * mean(c * dc) - 1)
  
}

x <- seq(from = 0, to = 100, by = 1)
y <- vector(mode = "double", length = length(x))

for (i in 1:length(x)) {
  y[i] <- kendall_tau(x[i])
}

png(
  filename = "graphs/kendall_tau.png",
  width = 30,
  height = 20,
  units = "cm",
  res = 300
)

plot(
  x,
  y,
  type = "l",
  lwd = 2,
  col = "steelblue4",
  ylab = TeX("$\\tau_C$"),
  xlab = TeX("$\\gamma$")
  )

dev.off()

spearman_rho <- function(gamma) {
  set.seed(42)
  omega <- sim_omega(1e4, gamma = gamma)
  
  c <- vector(mode = "double", length = length(vec.u))
  
  for (i in 1:length(vec.u)) {
    u <- vec.u[i]
    v <- vec.v[i]
    c[i] <- copula(u, v, omega$omega1, omega$omega2)
  }
  
  return(12 * mean(c) - 3)
  
}

rho <- vector(mode = "double", length = length(x))

for (i in 1:length(x)) {
  rho[i] <- spearman_rho(x[i])
}

png(
  filename = "graphs/spearman_rho.png",
  width = 30,
  height = 20,
  units = "cm",
  res = 300
)

plot(
  x,
  rho + 1,
  type = "l",
  lwd = 2,
  col = "steelblue4",
  ylab = TeX("$\\rho_C$"),
  xlab = TeX("$\\gamma$")
)

dev.off()
