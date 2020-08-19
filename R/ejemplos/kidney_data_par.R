# Probando el modelo

library(tidyverse)
files.sources = list.files("R/map")
sapply(files.sources, function(x) {source(paste0("R/map/", x))})

dt <- read_csv("data/kidney_data.csv")

dt_tidy <- dt %>%
  mutate(
    time = rep(c("t1", "t2"), times = nrow(.)/2),
    delta.name = rep(c("delta1", "delta2"), times = nrow(.)/2)
  ) %>%
  select(-c(age, disease, frailty)) %>% 
  pivot_wider(
    names_from = c(time, delta.name),
    values_from = c(t, delta)
  ) %>%
  magrittr::set_colnames(
    c("id", "sex", "t1", "t2", "delta1", "delta2")
  ) %>%
  mutate(
    sex = if_else(sex == 2, 0, sex)
  ) %>%
  identity()

system.time(
  prueba2 <- bayes_bisurv(
    dt = dt_tidy[, -1],
    alpha = 0.0001,
    beta = 0.0001,
    c = 1000,
    int.len = 10,
    iter = 500,
    burnin = 50
  )
)

# elapsed 265.977 

# plot(x = 1:38, y = prueba$omega2, pch = 2)
# points(x = 1:38, y = prueba$omega1)
# 
# tibble(omega1 = prueba$omega1, omega2 = prueba$omega2) %>%
#   mutate(id = 1:nrow(.)) %>%
#   ggplot(aes(x = id)) +
#   geom_segment(
#     aes(
#       y = omega1,
#       yend = omega2,
#       xend = id
#     ),
#     color = "gray50"
#   ) +
#   labs(x = "", y = "") +
#   geom_point(aes(y = omega1), shape = 1, color = "cornflowerblue") +
#   geom_point(aes(y = omega2), shape = 2, color = "cornflowerblue") +
#   theme_minimal()

plot(prueba2$theta.chain, type = "l")

t.max <- max(dt_tidy$t1, dt_tidy$t2)

t.part <- partition(t.max, int.len = 10)

plot.x <- seq(from = 0, to = 570, by = 0.1)
loc <- partition_location(plot.x, t.part)

f1 <- function(x) {
  return(prueba2$lambda1[x])
}

f2 <- function(x) {
  return(prueba2$lambda2[x])
}

plot(plot.x, f1(loc), type = "l", ylim = c(0, 0.04))
plot(plot.x, f2(loc), type = "l", ylim = c(0, 0.04))
