# Simulacion de la posterior de omega para h, y fijas.

# Se utiliza el algoritmo de Metropolis-Hastings.
# La distribucion candidata es W = Y + f con Y ~ Ga(1+y, 1+g).

library(ggplot2)

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

tib <- tibble::tibble(y = sim)

ggplot2::ggplot(tib, ggplot2::aes(x = 1:iter, y = y)) +
  ggplot2::geom_line(color = "steelblue1") +
  ggplot2::theme_minimal() +
  labs(y = "", x = "") +
  scale_x_continuous(labels = scales::comma)

acf(sim)

coda::effectiveSize(sim)


# Random Walk Metropolis ----------------------------------------

# Propuesta Gamma

rho <- function(x, omega, y, a, gamma) {
  l <- (log(x) - log(omega)) * (1 + y - a) -
    a*(1 - x/omega) - (1 + gamma)*(x-omega)
  
  return(exp(l))
  
}

sim2 <- vector(length = iter, mode = "double")

set.seed(42)

a <- y + 1

sim2[1] <- rgamma(n = 1, shape = a, scale = 1 + g) + h

for (i in 2:iter) {
  
  omega <- sim2[i-1]
  
  candidato <- rgamma(n = 1, shape = a, scale = a/omega) + h
  
  r <- min(rho(candidato, omega, y, a, g), 1)
  
  sim2[i] <- sim2[i - 1] + (candidato - sim2[i - 1]) * (runif(n = 1) < r)
  
}

tib <- tibble::tibble(y = sim2)

ggplot2::ggplot(tib, ggplot2::aes(x = 1:iter, y = y)) +
  ggplot2::geom_line(color = "steelblue1") +
  ggplot2::theme_minimal() +
  labs(y = "", x = "") +
  scale_x_continuous(labels = scales::comma)

acf(sim2)

coda::effectiveSize(sim2)

# Propuesta Uniforme

prop <- function(x, omega, y, gamma) {
  
  l <- y*(log(x) - log(omega)) - (1 + gamma)*(x - omega)
  
  return(exp(l))
  
}

sim3 <- vector(length = iter, mode = "double")

set.seed(42)

a <- y + 1

sim3[1] <- rgamma(n = 1, shape = a, scale = 1 + g) + h

for (i in 2:iter) {
  
  omega <- sim3[i-1]
  
  candidato <- runif(n = 1, min = max(h, omega - a), max = omega + a)
  
  r <- min(prop(candidato, omega, y, g), 1)
  
  sim3[i] <- sim3[i - 1] + (candidato - sim3[i - 1]) * (runif(n = 1) < r)
  
}

tib <- tibble::tibble(y = sim3)

ggplot2::ggplot(tib, ggplot2::aes(x = 1:iter, y = y)) +
  ggplot2::geom_line(color = "steelblue1") +
  ggplot2::theme_minimal() +
  labs(y = "", x = "") +
  scale_x_continuous(labels = scales::comma)

acf(sim3)

coda::effectiveSize(sim3)