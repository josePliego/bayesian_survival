###############################################################.
## Descripcion: Ejemplo de distribucion t como mezcla
##
## Autor: Jose Pliego
## Fecha: 2020-02-24
###############################################################.

f <- function(x, mu, sigma, nu) {
  tau <- 1/sigma
  constante <- tau*gamma((nu+1)*0.5)*(pi*nu)^(-1/2)/gamma(0.5*nu)
  
  constante * (1+(1/nu)*((x-mu)*tau)^2)^(-(nu+1)*0.5)
  
}

nsim <- 10e4
grados_libertad <- 100
mu <- 0
sigmasq <- .1

set.seed(1996)

chisq <- rchisq(nsim, grados_libertad)
y <- grados_libertad * sigmasq / chisq

x_y <- rnorm(nsim, mean = mu, sd = sqrt(y))

hist(x_y,
     probability = TRUE,
     col = "steelblue1",
     ylim = c(0, 1.3))

curve(f(x, mu, sqrt(sigmasq), grados_libertad),
      from = -1.532872,
      to = 1.374351,
      add = TRUE,
      col = "firebrick")


