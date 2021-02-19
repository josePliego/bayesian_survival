############################################################## .
## Capitulo 5: Simulaciones del modelo
##
## Autor: Jose Pliego
## Fecha: 2021-02-19
############################################################## .

if (!require("pacman")) install.packages("pacman")
pacman::p_load("BGPhazard", "readr", "parallel", "beepr")

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

system.time(runs <- mclapply(as.double(c(42, 44, 45)), f, mc.cores = 3))
write_rds(runs, "cache/runs.rds")

beep()
