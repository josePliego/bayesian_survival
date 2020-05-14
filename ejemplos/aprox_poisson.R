# Queremos simular de una distribucion Poisson suponiendo que no
# conocemos la constante exp(-\lambda)

tol <- 10e-30
x <- 0
acum <- 0
lambda <- 10
prob <- 1
f <- c()

while(prob > tol) {
 densidad <- x * log(lambda) - log(gamma(x + 1))
 densidad <- exp(densidad)
 acum <- acum + densidad
 prob <- densidad/acum
 f <- c(f, densidad)
 x <- x + 1
}

pois <- f / acum

muestra <- sample(1:x, size = 1e6, replace = TRUE, prob = pois)

hist(muestra, probability = TRUE)
points(1:x, dpois(1:x, lambda = 10))
