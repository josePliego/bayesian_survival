library(tidyverse)
library(profvis)

files.sources = list.files("R/model_functions_map/")
sapply(
  files.sources,
  function(x) {
    if (x != "map") source(paste0("R/model_functions_map/", x))
  }
)

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

profvis({
  bayes_bisurv(
    dt = select(dt_tidy, -id),
    alpha = 0.0001,
    beta = 0.0001,
    c = 1000,
    int.len = 10,
    iter = 500,
    burnin = 50
  )
})


plot(unlist(out$theta), type = "l")

lambda1_matrix <- do.call(cbind, out$lambda1)
omega1_matrix <- do.call(cbind, out$omega1)

plot(lambda1_matrix[1, ], type = "l")
plot(lambda1_matrix[2, ], type = "l")

plot(omega1_matrix[3, ], type = "l")
