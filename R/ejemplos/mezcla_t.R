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
mu <- 1
sigmasq <- 1

set.seed(42)

chisq <- rchisq(nsim, grados_libertad)
y <- grados_libertad * sigmasq / chisq

x_y <- rnorm(nsim, mean = mu, sd = sqrt(y))

png(
  filename = "graphs/sim_mezcla.png",
  width = 30,
  height = 20,
  units = "cm",
  res = 300
)

hist(
  x_y,
  probability = TRUE,
  col = viridis::viridis(2)[[2]],
  border = viridis::viridis(2)[[2]],
  ylim = c(0, 0.6),
  breaks = seq(from = -4, to = max(x_y) + 0.05, by = 0.05),
  main = "",
  xlab = "",
  ylab = ""
  )

curve(f(x, mu, sqrt(sigmasq), grados_libertad),
      from = -4,
      to = max(x_y),
      add = TRUE,
      col = viridis::viridis(2)[[1]],
      lwd = 2
      )

dev.off()
