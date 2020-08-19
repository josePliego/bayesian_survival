library(tidyverse)
library(profvis)
library(parallel)
library(patchwork)

files.sources = list.files("R/model_functions_map/")
sapply(
  files.sources,
  function(x) {
    if (x != "map") source(paste0("R/model_functions_map/", x))
  }
)

source("R/lib/acceptance_rate.R")

dt_tidy <- read_rds("data/dt_kidney_tidy.rds")

f <- function(seed) {
  bayes_bisurv(
  dt = select(dt_tidy, -id),
  alpha = 0.0001,
  beta = 0.0001,
  c = 1000,
  int.len = 10,
  iter = 1000,
  gamma.d = 0.2,
  burnin = 0,
  seed = seed
  )
  }

out <- list()

out[[1]] <- f(42)
beepr::beep()

theta.chain <- unlist(out[[1]]$theta)
plot(theta.chain, type = "l")
acceptance_rate(theta.chain[500:1000])

gamma.chain <- unlist(out[[1]]$gamma)
plot(gamma.chain, type = "l")
acceptance_rate(gamma.chain[500:1000])

# Acceptance rate para 1000 iteraciones, seed = 42
### THETA
# sd = 1 ... ar = 0.081
# sd = 0.5 ... ar = 0.183
# sd = 0.3 ... ar = 0.234
### GAMMA
# gamma.d = 0.1 ... ar = 0.59
# gamma.d = 0.2 ... ar = 0.363

omega1.1 <- vector(mode = "double", length = 1000L)

for (i in 1:1000) {
  omega1.1[i] <- out[[1]]$omega1[[i]][1]
}

# Omega 1 ----------------------------------------------------------------

names(o4$omega1) <- paste0("iter.", 1:1000)

o1 <- o4$omega1 %>%
  as_tibble() %>%
  mutate(
    individuo = 1:38
  ) %>%
  pivot_longer(cols = iter.1:iter.1000, names_to = "t", values_to = "obs") %>%
  separate(t, into = c("iter", "t"), sep = "\\.") %>%
  mutate(t = as.double(t))

o1 %>%
  ggplot(aes(x = t, y = obs)) +
  geom_line() +
  facet_wrap(~ individuo, scales = "free_y")

o1 %>%
  filter(individuo == 21) %>%
  ggplot(aes(x = t, y = obs)) +
  geom_line()


# Omega 2 ----------------------------------------------------------------

names(o4$omega2) <- paste0("iter.", 1:5000)

o1 <- o4$omega2 %>%
  as_tibble() %>%
  mutate(
    individuo = 1:38
  ) %>%
  pivot_longer(cols = iter.1:iter.5000, names_to = "t", values_to = "obs") %>%
  separate(t, into = c("iter", "t"), sep = "\\.") %>%
  mutate(t = as.double(t))

o1 %>%
  ggplot(aes(x = t, y = obs)) +
  geom_line() +
  facet_wrap(~ individuo, scales = "free_y")

o1 %>%
  filter(individuo == 21) %>%
  ggplot(aes(x = t, y = obs)) +
  geom_line()

# T1 ----------------------------------------------------------------

names(o4$t1) <- paste0("iter.", 1:5000)

o1 <- o4$t1 %>%
  as_tibble() %>%
  mutate(
    individuo = 1:38
  ) %>%
  pivot_longer(cols = iter.1:iter.5000, names_to = "t", values_to = "obs") %>%
  separate(t, into = c("iter", "t"), sep = "\\.") %>%
  mutate(t = as.double(t))

o1 %>%
  ggplot(aes(x = t, y = obs)) +
  geom_line() +
  facet_wrap(~ individuo, scales = "free_y")

o1 %>%
  filter(individuo == 21) %>%
  ggplot(aes(x = t, y = obs)) +
  geom_line()

# t2 ----------------------------------------------------------------

names(o4$t2) <- paste0("iter.", 1:5000)

o1 <- o4$t2 %>%
  as_tibble() %>%
  mutate(
    individuo = 1:38
  ) %>%
  pivot_longer(cols = iter.1:iter.5000, names_to = "t", values_to = "obs") %>%
  separate(t, into = c("iter", "t"), sep = "\\.") %>%
  mutate(t = as.double(t))

o1 %>%
  ggplot(aes(x = t, y = obs)) +
  geom_line() +
  facet_wrap(~ individuo, scales = "free_y")

o1 %>%
  filter(individuo == 21) %>%
  ggplot(aes(x = t, y = obs)) +
  geom_line()


###############################################################.

## Seccion: SUPUESTO PROP HAZ ####

###############################################################.

library(survival)

s1 <- Surv(time = dt_tidy$t1, event = dt_tidy$delta1)
s2 <- Surv(time = dt_tidy$t2, event = dt_tidy$delta2)

km1 <- survfit(s1 ~ dt_tidy$sex)

t_hombre <- km1$time[1:28]
t_mujer <- km1$time[29:38]

surv_hombre <- km1$surv[1:28]
surv_mujer <- km1$surv[29:38]

plot(x = log(t_hombre), y = log(-log(surv_hombre)), type = "l")
lines(x = log(t_mujer), y = log(-log(surv_mujer)))

km2 <- survfit(s1 ~ dt_tidy$sex)

t_hombre <- km2$time[1:28]
t_mujer <- km2$time[29:38]

surv_hombre <- km2$surv[1:28]
surv_mujer <- km2$surv[29:38]

plot(x = log(t_hombre), y = log(-log(surv_hombre)), type = "l")
lines(x = log(t_mujer), y = log(-log(surv_mujer)))
