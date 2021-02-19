############################################################## .
## Capitulo 5: Simulacion copula
##
## Autor: Jose Pliego
## Fecha: fecha
############################################################## .

## Seccion: 1. Preambulo ####

############################################################### .

if (!require("pacman")) install.packages("pacman")
pacman::p_load("viridis")

source("R/lib/copula.R")

set.seed(42)

omega <- sim_omega(1e4, gamma = 5)
secuencia <- seq(from = 0.01, to = 0.99, by = 0.01)
grid <- matrix(nrow = length(secuencia), ncol = length(secuencia))

for (u in 1:length(secuencia)) {
  
  for (v in 1:length(secuencia)) {
    
    grid[u, v] <- copula(secuencia[u], secuencia[v], omega$omega1, omega$omega2)
    
  }
}

png(
  filename = "graphs/copula_5.png",
  width = 30,
  height = 30,
  units = "cm",
  res = 300
)

persp(
  secuencia,
  secuencia,
  grid,
  phi = 0,
  theta = 135,
  xlab = "u",
  ylab = "v",
  zlab = "C",
  col = viridis(2)[[2]],
  border = viridis(2)[[1]]
)

dev.off()

png(
  filename = "graphs/copula_5_contour.png",
  width = 30,
  height = 30,
  units = "cm",
  res = 300
)

filled.contour(
  secuencia,
  secuencia,
  grid,
  color.palette = viridis
)

dev.off()
