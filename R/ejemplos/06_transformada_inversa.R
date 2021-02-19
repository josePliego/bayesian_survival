############################################################## .
## Capitulo 4: Metodo de la transformacion inversa
##
## Autor: Jose Pliego
## Fecha: 2021-02-19
############################################################## .

if (!require("pacman")) install.packages("pacman")
pacman::p_load("viridis")

f <- function(x, lambda) lambda * exp(-lambda * x)

dist_inv <- function(y, lambda) -(1/lambda) * log(1 - y)

set.seed(42)
nsim <- 10e4
lambda <- 0.5

u <- runif(nsim)
x <- dist_inv(u, lambda)

png(
  filename = "graphs/transformada_inversa.png",
  width = 30,
  height = 20,
  units = "cm",
  res = 300
)

hist(
  x,
  probability = TRUE,
  col = viridis(2)[[2]],
  border = viridis(2)[[2]],
  xlim = c(0, 20),
  breaks = seq(from = 0, to = 24, by = 0.2),
  xlab = "",
  ylab = "",
  main = ""
)
curve(
  f(x, lambda),
  from = 0,
  to = max(x),
  add = TRUE,
  col = viridis(2)[[1]],
  lwd = 2
)

dev.off()
