library(BGPhazard)
library(survival)
library(tidyverse)
library(patchwork)

?BSBInit

set.seed(42)
t <- rgamma(n = 20, shape = 2)

init <- BSBInit(t1 = Surv(t), t2 = Surv(t), part_len = 0.5, seed = 42)

s1 <- BSBHaz(init, 300, 100, gamma_d = 2.5, omega_d = 1)

BSBSumm(s1, "gamma")
BSBPlotDiag(s1, "gamma", "ergodic_means")
BSBPlotDiag(s1, "gamma", "traceplot")

t2 <- rgamma(n = 20, shape = 2)
init2 <- BSBInit(t1 = Surv(t), t2 = Surv(t2), part_len = 0.5, seed = 43)
s2 <- BSBHaz(init2, 3000, 900, gamma_d = 1.5, omega_d = 1)

BSBSumm(s2, "gamma")
BSBPlotDiag(s2, "gamma", "ergodic_means")
BSBPlotDiag(s2, "gamma", "traceplot")
