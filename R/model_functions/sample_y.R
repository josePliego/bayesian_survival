# Sampling latent vector y
log_density_y <- function(y, omega1, omega2, gamma) {
  g <- y * (log(gamma) + log(1 + gamma) + log(omega1) + log(omega2)) -
    lgamma(1 + y) - lgamma(2 + y)

  return(g)
}

probabilities_y <- function(omega1, omega2, gamma) {

  prob_list <- vector(mode = "list", length = length(omega1))

  for (i in 1:length(prob_list)) {

    y <- vector(mode = "numeric", length = 1e6L)
    acum <- 0
    j <- 0
    prob <- 1

    while (j <= 1e2 & prob > 1e-6) {
      gj <- exp(log_density_y(j, omega1[i], omega2[i], gamma))
      if (is.nan(gj)) {break}
      prueba.acum <- acum + gj
      if (is.infinite(prueba.acum)) {break}
      if (prueba.acum == 0) {break}
      acum <- prueba.acum
      prob <- gj / acum
      y[j + 1] <- gj
      j <- j + 1
    }

    y <- y[1:j] / acum
    prob_list[[i]] <- cumsum(y)

  }

  return(prob_list)

}

sample_y <- function(omega1, omega2, gamma) {

  dist.list <- probabilities_y(omega1, omega2, gamma)

  y <- vector(mode = "double", length = length(dist.list))

  for (i in 1:length(y)) {

    dist <- dist.list[[i]]
    u <- runif(n = 1)
    index <- 1

    while (u > dist[index]) {
      index <- index + 1
    }

    y[i] <- index - 1

  }

  return(y)

}
