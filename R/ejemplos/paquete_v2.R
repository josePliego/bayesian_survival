# devtools::install_github("EAMI91/BGPhazard")

library(BGPhazard)
library(survival)
library(tidyverse)
library(patchwork)

KIDNEY
# init <- BSBInit(select(KIDNEY, -sex), part_len = 10, seed = 42)
init1 <- BSBInit(KIDNEY, part_len = 10, seed = 42)
init2 <- BSBInit(KIDNEY, part_len = 10, seed = 43)
init3 <- BSBInit(KIDNEY, part_len = 10, seed = 44)
init4 <- BSBInit(KIDNEY, part_len = 10, seed = 45)
samples1 <- BSBSam(
  init1,
  iter = 50000,
  burn_in = 10000,
  gamma_d = 10,
  theta_d = 0.3,
  omega_d = 5.4,
  seed = 42
  )
beepr::beep()
samples2 <- BSBSam(
  init2,
  iter = 50000,
  burn_in = 10000,
  gamma_d = 10,
  theta_d = 0.3,
  omega_d = 5.4,
  seed = 42
)
samples3 <- BSBSam(
  init3,
  iter = 50000,
  burn_in = 10000,
  gamma_d = 10,
  theta_d = 0.3,
  omega_d = 5.4,
  seed = 42
)
samples4 <- BSBSam(
  init4,
  iter = 50000,
  burn_in = 10000,
  gamma_d = 10,
  theta_d = 0.3,
  omega_d = 5.4,
  seed = 42
)

write_rds(samples1, "cache/samples1.rds")
write_rds(samples2, "cache/samples2.rds")
write_rds(samples3, "cache/samples3.rds")
write_rds(samples4, "cache/samples4.rds")

BSBPlotDiag(samples1, "lambda1", "ergodic_means")
# BSBPlotDiag(samples, "lambda2", "ergodic_means")
# BSBPlotDiag(samples, "omega1", "ergodic_means")
# BSBPlotDiag(samples, "omega1", "traceplot")
# BSBPlotDiag(samples, "omega2", "ergodic_means")
# BSBPlotDiag(samples, "omega2", "traceplot")
BSBPlotSumm(samples1, "lambda1")
BSBPlotSumm(samples3, "lambda2")
# BSBPlotSumm(samples, "s1")
# BSBPlotSumm(samples, "s2")
# BSBSumm(samples, "s1")
# BSBSumm(samples, "theta")
# BSBPlotDiag(samples, "theta", "traceplot")
# BSBPlotDiag(samples, "theta", "ergodic_means")
# 
# km1 <- survfit(Surv(KIDNEY$t1, KIDNEY$delta1) ~ KIDNEY$sex)
# plot(km1)
# km2 <- survfit(Surv(KIDNEY$t2, KIDNEY$delta2) ~ KIDNEY$sex)
# plot(km2)
# 
# h1 <- BSBPlotSumm(samples, "lambda1")
# h2 <- BSBPlotSumm(samples, "lambda2")
# h1/h2 +
#   ggsave("graphs/summaries_hazard.png", width = 15, height = 20, units = "cm")
# 
# s1 <- BSBPlotSumm(samples, "s1")
# s2 <- BSBPlotSumm(samples, "s2")
# s1/s2 +
#   ggsave("graphs/summaries_survival.png", width = 15, height = 20, units = "cm")
