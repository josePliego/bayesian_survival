log_density_gamma <- function(gamma, proposal, omega1, omega2, y) {

  n <- length(omega1)
  sum.y <- sum(y)
  sum.omega <- sum(omega1) + sum(omega2)

  l <- (2*n - 2) * (log(1 + proposal) - log(1 + gamma)) +
    sum.y * (log(proposal) - log(gamma) + log(1 + proposal) - log(1 + gamma)) -
    sum.omega * (proposal - gamma)

  return(l)

}

sample_gamma <- function(gamma, omega1, omega2, y, gamma.d) {
  
  proposal <- runif(n = 1, min = max(gamma - gamma.d, 0), max = gamma + gamma.d)

  l.density <- log_density_gamma(gamma, proposal, omega1, omega2, y)

  alpha <- min(exp(l.density), 1)

  u <- runif(n = 1)

  gamma.out <- gamma + (proposal - gamma) * (u <= alpha)

  return(gamma.out)

}
