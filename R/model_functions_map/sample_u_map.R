log_density_u <- function(
  u,
  lambda.k,
  lambda.k1,
  alpha,
  beta,
  c,
  index.indicator
) {
  # index.indicator <- 0 if the index is the last one.
  
  pi <- u * (log(c) + index.indicator * log(c + beta) +
               log(lambda.k) + index.indicator * log(lambda.k1)) -
    lgamma(u + 1) - lgamma(alpha + u) * index.indicator
  
  return(pi)
  
}

prob_u <- function(lambda, lambda1, alpha, beta, c, index.indicator) {
  
  u <- vector(mode = "numeric", length = 1e3L)
  acum <- 0
  j <- 0
  prob <- 1
  
  while (j <= 5e2 & prob > 1e-6) {
    pi.j <- exp(
      log_density_u(j, lambda, lambda1, alpha, beta, c, index.indicator)
    )
    if (is.nan(pi.j)) {break}
    prueba.acum <- acum + pi.j
    if (is.infinite(prueba.acum)) {break}
    if (prueba.acum == 0) {break}
    acum <- prueba.acum
    prob <- pi.j / acum
    u[j + 1] <- pi.j
    j <- j + 1
  }
  
  u <- u[1:j] / acum
  prob_fun <- cumsum(u)
  
  return(prob_fun)
  
}

sample_u <- function(lambda, lambda1, alpha, beta, c, index.indicator) {
  
  distribution <- prob_u(lambda, lambda1, alpha, beta, c, index.indicator)
  
  if(is.na(distribution[1])) {
    distribution <- c(1)
    warning("No distribution for U")
  }
  
  u <- runif(n = 1)
  index <- 1
  
  while (u > distribution[index]) {
    index <- index + 1
  }
  
  y <- index - 1
  
  return(y)
  
}
