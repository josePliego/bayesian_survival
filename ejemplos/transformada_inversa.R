###############################################################.
## Descripcion: Metodo de la transformada inversa
##
## Autor: Jose Pliego
## Fecha: 2020-02-21
###############################################################.

## Seccion: 1. Distribucion exponencial ####

###############################################################.

f <- function(x, lambda) lambda * exp(-lambda * x)

dist_inv <- function(y, lambda) -(1/lambda) * log(1-y) 

set.seed(1996)
nsim <- 10e4
lambda <- 0.5

u <- runif(nsim)

x <- dist_inv(u, lambda)

hist(x,
     probability = TRUE,
     col = "steelblue1")

curve(f(x, lambda),
      from = 0,
      to = max(x),
      add = TRUE,
      col = "firebrick")
