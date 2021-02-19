############################################################## .
## Capitulo 2: Estimador Kaplan-Meier
##
## Autor: Jose Pliego
## Fecha: 2021-02-19
############################################################## .

## Seccion: 1. Preambulo ####

############################################################### .

# Paquetes necesarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load("survival", "latex2exp", "viridis")

dt_aml <- survival::aml
times <- Surv(dt_aml$time, dt_aml$status)

km1 <- survfit(times ~ 1, conf.type = "none")
km2 <- survfit(times ~ dt_aml$x, conf.type = "none")

png(
  filename = "graphs/km1.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
  )

par(mar = c(5, 6, 4, 1) + .1)
plot(
  km1,
  lwd = 1.5,
  conf.int = FALSE,
  col = viridis(1),
  xlab = TeX("t"),
  ylab = TeX("\\hat{S}(t)")
  )

dev.off()

png(
  filename = "graphs/km2.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
  )

par(mar = c(5, 6, 4, 1) + .1)
plot(
  km2,
  conf.int = FALSE,
  lwd = 1.5,
  col = viridis(2),
  xlab = TeX("t"),
  ylab = TeX("\\hat{S}(t)")
  )

dev.off()
