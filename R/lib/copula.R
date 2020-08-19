sim_omega <- function(n.sim, a = 2, b = 1, gamma = 5) {
  lam <- rgamma(n = n.sim, shape = a, rate = b)
  y <- rpois(n = n.sim, lambda = lam * gamma)
  omega1 <- rgamma(n = n.sim, shape = a + y, rate = b + gamma)
  omega2 <- rgamma(n = n.sim, shape = a + y, rate = b + gamma)
  
  return(list("omega1" = omega1, "omega2" = omega2))
}

copula <- function(u, v, omega1, omega2) {
  c <- (1 + log(1 - u) / omega1) * (1 + log(1 - v) / omega2)
  c <- mean(c * (u < 1 - exp(-omega1)) * (v < 1 - exp(-omega2)))
  res <- (u + v - 1 + c) * (u > 0) * (v > 0)
  return(res)
}

densidad_copula <- function(u, v, omega1, omega2) {
  
  c <- 1 / (1 - u) * 1 / (1 - v) * 1 / (omega1 * omega2)
  c <- mean(c * (u < 1 - exp(-omega1)) * (v < 1 - exp(-omega2)))
  
  return(c)
  
}