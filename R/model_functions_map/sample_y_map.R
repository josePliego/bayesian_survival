log_density_y <- function(y, omega1, omega2, gamma) {
  
  g <- y * (log(gamma) + log(1 + gamma) + log(omega1) + log(omega2)) -
    lgamma(1 + y) - lgamma(2 + y)
  
  return(g)
  
}

prob_y <- function(omega1, omega2, gamma) {
  
  y <- vector(mode = "numeric", length = 1e3L)
  acum <- 0
  j <- 0
  prob <- 1
  
  while (j <= 5e2 & prob > 1e-6) {
    gj <- exp(log_density_y(j, omega1, omega2, gamma))
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
  prob_fun <- cumsum(y)
  
  return(prob_fun)
  
}

sample_y <- function(omega1, omega2, gamma) {
  
  distribution <- prob_y(omega1, omega2, gamma)
  
  if(is.na(distribution[1])) {
    distribution <- c(1)
    warning("No distribution for Y")
  }
  
  u <- runif(n = 1)
  index <- 1
  
  while (u > distribution[index]) {
    index <- index + 1
  }
  
  y <- index - 1
  
  return(y)
  
}