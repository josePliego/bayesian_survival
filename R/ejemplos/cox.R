library(survival)
library(latex2exp)
library(tidyverse)

dt_aml <- survival::aml

times <- Surv(dt_aml$time, dt_aml$status)


# Kaplan Meier estimators -------------------------------------------------

km1 <- survfit(times ~ 1, conf.type = "none")
km2 <- survfit(times ~ dt_aml$x, conf.type = "none")

png(filename = "graphs/km1.png",
    width = 25,
    height = 20,
    units = "cm",
    res = 300)
par(mar=c(5,6,4,1)+.1)
plot(
  km1,
  lwd = 1.5,
  conf.int = FALSE,
  col = viridis::viridis(1),
  xlab = TeX("t"),
  ylab = TeX("\\hat{S}(t)"))

dev.off()

png(filename = "graphs/km2.png",
    width = 25,
    height = 20,
    units = "cm",
    res = 300)
par(mar=c(5,6,4,1)+.1)
plot(
  km2,
  conf.int = FALSE,
  lwd = 1.5,
  col = viridis::viridis(2),
  xlab = TeX("t"),
  ylab = TeX("\\hat{S}(t)"))
dev.off()


# Cox PH ------------------------------------------------------------------

# Prop Haz assumption

t_piloto <- km2$time[1:10]
t_control <- km2$time[11:length(km2$time)]

surv_piloto <- km2$surv[1:10]
surv_control <- km2$surv[11:length(km2$time)]

png(filename = "graphs/prop_haz_km.png",
    width = 25,
    height = 20,
    units = "cm",
    res = 300)
par(mar=c(5,5,1,1)+.1)
plot(
  log(t_piloto),
  log(-log(surv_piloto)),
  type = "s",
  ylim = c(-3, 0.9),
  xlim = c(log(5), 5),
  col = viridis::viridis(1),
  xlab = TeX("log(t)"),
  ylab = TeX("log(-log \\hat{S}(t))")
  )
lines(
  log(t_control),
  log(-log(surv_control)),
  type = "s",
  col = viridis::viridis(2)[[2]]
  )
dev.off()

dt_transform <- dt_aml %>%
  mutate(
    z = case_when(
      x == "Maintained" ~ 1,
      x == "Nonmaintained" ~ 0
    )
  )

model <- coxph(times ~ dt_transform$z)
summary(model)
