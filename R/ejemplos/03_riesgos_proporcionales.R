############################################################## .
## Capitulo 2: Modelo de riesgos proporcionales
##
## Autor: Jose Pliego
## Fecha: 2021-02-19
############################################################## .

# Paquetes necesarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load("survival", "latex2exp", "tidyverse", "viridis")

dt_aml <- survival::aml
times <- Surv(dt_aml$time, dt_aml$status)

km <- survfit(times ~ dt_aml$x, conf.type = "none")

t_piloto <- km$time[1:10]
t_control <- km$time[11:length(km$time)]

surv_piloto <- km$surv[1:10]
surv_control <- km$surv[11:length(km$time)]

# Grafica para revisar supuesto de proporcionalidad
png(
  filename = "graphs/prop_haz_km.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
  )

par(mar = c(5, 5, 1, 1) + .1)
plot(
  log(t_piloto),
  log(-log(surv_piloto)),
  type = "s",
  ylim = c(-3, 0.9),
  xlim = c(log(5), 5),
  col = viridis(1),
  xlab = TeX("log(t)"),
  ylab = TeX("log(-log \\hat{S}(t))")
)
lines(
  log(t_control),
  log(-log(surv_control)),
  type = "s",
  col = viridis(2)[[2]]
)

dev.off()

# Ajuste del modelo de riesgos proporcionales
dt_transform <-
  dt_aml %>%
  mutate(
    z = case_when(
      x == "Maintained" ~ 1,
      x == "Nonmaintained" ~ 0
      )
    )

model <- coxph(times ~ dt_transform$z)
summary(model)
