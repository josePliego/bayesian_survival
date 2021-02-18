# devtools::install_github("EAMI91/BGPhazard")

library(BGPhazard)
library(survival)
library(tidyverse)
library(patchwork)
library(parallel)

KIDNEY
f <- function(seed) {
  init <- BSBInit(KIDNEY, part_len = 10, seed = seed)
  samples <- BSBHaz(
    init,
    iter = 100000,
    burn_in = 10000,
    gamma_d = 0.70,
    theta_d = 0.35,
    omega_d = 5.45,
    seed = seed
    )
  
  return(samples)
}

system.time(runs <- mclapply(as.double(c(42, 44, 45, 46)), f, mc.cores = 4))
write_rds(runs, "cache/runs.rds")

BSBSumm(runs[[1]], "gamma")
BSBSumm(runs[[2]], "gamma")
BSBSumm(runs[[3]], "gamma")
BSBSumm(runs[[4]], "gamma")
BSBSumm(runs[[5]], "gamma")
BSBPlotDiag(runs[[1]], "gamma", "ergodic_means")
BSBPlotDiag(runs[[1]], "gamma", "traceplot")
BSBPlotDiag(runs[[2]], "gamma", "ergodic_means")
BSBPlotDiag(runs[[2]], "gamma", "traceplot")
BSBPlotDiag(runs[[3]], "gamma", "ergodic_means")
BSBPlotDiag(runs[[3]], "gamma", "traceplot")
BSBPlotDiag(runs[[4]], "gamma", "ergodic_means")
BSBPlotDiag(runs[[4]], "gamma", "traceplot")
BSBPlotDiag(runs[[5]], "gamma", "ergodic_means")
BSBPlotDiag(runs[[5]], "gamma", "traceplot")

BSBSumm(runs[[1]], "theta")
BSBSumm(runs[[2]], "theta")
BSBSumm(runs[[3]], "theta")
BSBSumm(runs[[4]], "theta")
BSBSumm(runs[[5]], "theta")
BSBSumm(runs2[[1]], "theta")
BSBSumm(runs2[[2]], "theta")
BSBPlotDiag(runs[[1]], "theta", "ergodic_means")
BSBPlotDiag(runs[[1]], "theta", "traceplot")
BSBPlotDiag(runs[[2]], "theta", "ergodic_means")
BSBPlotDiag(runs[[2]], "theta", "traceplot")
BSBPlotDiag(runs[[3]], "theta", "ergodic_means")
BSBPlotDiag(runs[[3]], "theta", "traceplot")
BSBPlotDiag(runs[[4]], "theta", "ergodic_means")
BSBPlotDiag(runs[[4]], "theta", "traceplot")

runs[[5]] <- f(47)

system.time(runs2 <- mclapply(as.double(c(48, 49)), f, mc.cores = 2))
write_rds(runs2, "cache/runs2.rds")
beepr::beep()

BSBSumm(runs2[[1]], "gamma")
BSBSumm(runs2[[2]], "gamma")
BSBPlotDiag(runs2[[1]], "gamma", "ergodic_means")
BSBPlotDiag(runs2[[1]], "gamma", "traceplot")
BSBPlotDiag(runs2[[2]], "gamma", "ergodic_means")
BSBPlotDiag(runs2[[2]], "gamma", "traceplot")
