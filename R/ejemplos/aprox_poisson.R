# Queremos simular de una distribución Poisson suponiendo que no
# conocemos la constante exp(-\lambda)

tol <- 10e-30
x <- 0
acum <- 0
lambda <- 10
prob <- 1
f <- c()

while(prob > tol) {
 densidad <- x * log(lambda) - lgamma(x + 1)
 densidad <- exp(densidad)
 acum <- acum + densidad
 prob <- densidad/acum
 f <- c(f, densidad)
 x <- x + 1
}

pois <- f / acum

set.seed(42)

muestra <- sample(1:x, size = 1e6, replace = TRUE, prob = pois)

png(
    filename = "graphs/poisson.png",
    width = 25,
    height = 20,
    units = "cm",
    res = 300
    )

hist(
    muestra,
    probability = TRUE,
    xlab = "",
    main = "",
    col = "lightsteelblue1",
    border = "lightsteelblue1",
    ylab = ""
    )

points(
    1:x,
    dpois(1:x, lambda = 10),
    col = "steelblue4",
    pch = 19
    )

dev.off()
