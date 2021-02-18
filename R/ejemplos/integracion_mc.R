###############################################################.
## Descripcion: Ejemplo de integracion MC
##
## Autor: Jose Pliego
## Fecha: 2020-02-26
###############################################################.

library(dplyr)
library(scales)

# Utilizando integracion Monte Carlo buscamos aproximar la integral
# \int_{0}^2 g(x) dx, donde g(x) = x^4
# Analiticamente es facil ver que esta integral vale 6.4
#
# Utilizaremos primero una distribucion U(0,2) y luego una distribucion
# bivariada uniforme en [0, 2] X [0, 16]

g <- function(x) x^4

nsim <- 10e4

# U(0,2)

set.seed(1996)

x <- runif(nsim,
           min = 0,
           max = 2)

theta_hat <- 2 * mean(g(x))

print(theta_hat)
print(abs(theta_hat - 0.2*32))

x_serie <- 2 * cumsum(g(x))/1:nsim

plot(1:nsim,
     x_serie,
     type = "l",
     ylim = c(4, 7),
     col = "steelblue")

abline(h = 0.2*32,
       col = "firebrick",
       lty = 2)

# Bivariada

y <- runif(nsim,
           min = 0,
           max = 16)

exito <- if_else(y <= g(x), 1, 0)

theta_hat2 <- mean(exito) * 32

print(theta_hat2)
print(abs(theta_hat2 - 0.2*32))

png(
  filename = "graphs/sim_hx.png",
  width = 10,
  height = 7,
  units = "cm",
  res = 300
  )

plot(
  x,
  y,
  col = alpha(if_else(y <= g(x), viridis::viridis(3)[[1]], viridis::viridis(3)[[2]]), 0.5),
  pch = 20,
  xlab = "",
  ylab = ""
  )

curve(g(x),
      from = 0,
      to = 2, 
      col = viridis::viridis(3)[[3]],
      add = TRUE,
      lwd = 2)

dev.off()

dev.off()

curve(g(x),
      from = 0,
      to = 2, 
      col = "black",
      add = FALSE,
      lwd = 2,
      ylab = "h(x)")

png(filename = "graphs/hx.png",
    width = 10,
    height = 7,
    units = "cm",
    res = 300)
curve(g(x),
      from = 0,
      to = 2, 
      col = "black",
      add = FALSE,
      lwd = 2,
      ylab = "h(x)",
      xlim = c(0, 2),
      ylim = c(0, 17))
dev.off()

# ggplot2::ggsave("graphs/hx.png", width = 10, height = 7, units = "cm")


