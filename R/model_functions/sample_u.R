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

probabilities_u <- function(lambda, alpha, beta, c){

  prob_list <- vector(mode = "list", length = length(lambda))

  for (i in 1:length(prob_list)) {

    u <- vector(mode = "numeric", length = 1e6L)
    acum <- 0
    j <- 0
    prob <- 1

    index.ind <- 0 + 1 * (i < length(prob_list))
    lambda.k <- lambda[i]
    lambda.k1 <- lambda[i + index.ind]

    while (j <= 1e2 & prob > 1e-6) {
      pi.j <- exp(
        log_density_u(j, lambda.k, lambda.k1, alpha, beta, c, index.ind)
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
    prob_list[[i]] <- cumsum(u)

  }

  return(prob_list)

}

sample_u <- function(lambda, alpha, beta, c) {

  dist.list <- probabilities_u(lambda, alpha, beta, c)

  u <- vector(mode = "double", length = length(dist.list))

  for (i in 1:length(u)) {

    dist <- dist.list[[i]]
    unif <- runif(n = 1)
    index <- 1

    while (unif > dist[index]) {
      index <- index + 1
    }

    u[i] <- index - 1

  }

  return(u)

}