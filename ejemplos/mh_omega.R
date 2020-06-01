# Simulacion de la posterior de omega para h, y fijas.

# Se utiliza el algoritmo de Metropolis-Hastings.
# La distribucion candidata es W = Y + f con Y ~ Ga(1+y, 1+g).

iter <- 1e6
y <- 2
h <- 4
g <- 1

sim <- vector(length = iter, mode = "double")

q <- function(propuesta, anterior) {
  
  l <- y * (log(propuesta) -
              log(propuesta - h) -
              log(anterior) +
              log(anterior - h))
  
  return(exp(l))
  
}

set.seed(42)

sim[1] <- rgamma(n = 1, shape = 1 + y, scale = 1 + g)

for (i in 2:iter) {
  
  candidato <- rgamma(n = 1, shape = 1 + y, scale = 1 + g) + h
  
  rho <- min(q(candidato, sim[i - 1]), 1)
  
  sim[i] <- sim[i - 1] + (candidato - sim[i - 1]) * (runif(n = 1) < rho)
  
}

plot(1:iter, sim, type = "l")

hist(sim, probability = TRUE)

tib <- tibble::tibble(y = sim)

ggplot2::ggplot(tib, ggplot2::aes(x = 1:iter, y = y)) +
  ggplot2::geom_line() +
  ggplot2::theme_minimal()

acf(sim)

coda::effectiveSize(sim) / length(sim)
