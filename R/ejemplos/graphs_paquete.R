library(tidyverse)
library(latex2exp)
library(patchwork)
library(viridis)

out <- read_rds("cache/runs.rds")

out1 <- out[[1]]
out2 <- out[[2]]
out3 <- out[[3]]


# Theta -------------------------------------------------------------------

colores <- c(
  "chain1" = viridis(3)[[1]],
  "chain2" = viridis(3)[[2]],
  "chain3" = viridis(3)[[3]]
  )
etiquetas <- c(
  "chain1" = "Cadena 1", "chain2" = "Cadena 2", "chain3" = "Cadena 3"
  )

theta <- tibble(
  chain1 = out1$theta[1, ],
  chain2 = out2$theta[1, ],
  chain3 = out3$theta[1, ]
  ) %>%
  mutate(t = 1:90000) %>%
  pivot_longer(cols = -t)

ggplot(theta, aes(x = t, y = value, color = name)) +
  geom_line(alpha = 0.5, size = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = colores, labels = etiquetas) +
  labs(y = TeX("$\\theta$")) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.title = element_blank(), legend.position = "none") +
  ggsave("graphs/theta_traceplot.png", width = 20, height = 15, units = "cm")

ggplot(theta, aes(x = value)) +
  geom_density(alpha = 0.3, color = viridis(1), fill = viridis(1)) +
  # scale_fill_manual(values = colores, labels = etiquetas) +
  # scale_color_manual(values = colores) +
  # guides(color = FALSE) +
  labs(x = TeX("$\\theta$"), y = TeX("$\\pi(\\theta | data)$")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
    ) +
  xlim(c(-2, 2)) +
  ggsave("graphs/theta_densities.png", width = 14, height = 10, units = "cm")

theta %>%
  group_by(name) %>%
  mutate(value = cumsum(value) / 1:90000) %>%
  ungroup() %>%
  ggplot(aes(x = t, y = value, color = name)) +
  geom_line(size = 0.7) +
  scale_color_manual(values = colores, labels = etiquetas) +
  scale_x_continuous(labels = scales::comma) +
  labs(y = TeX("$\\hat{\\mu}_{\\theta}$")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "none") +
  ylim(c(0, 1)) +
  ggsave("graphs/theta_ergodic_means.png", width = 14, height = 10, units = "cm")


# Gamma -------------------------------------------------------------------

# colores <- c("chain1" = "deepskyblue1", "chain2" = "dodgerblue1",
#              "chain3" = "steelblue1", "chain4" = "lightskyblue")
# etiquetas <- c("chain1" = "Cadena 1", "chain2" = "Cadena 2",
#                "chain3" = "Cadena 3", "chain4" = "Cadena 4")
gamma <- tibble(
  chain1 = out1$gamma[1, ],
  chain2 = out2$gamma[1, ],
  chain3 = out3$gamma[1, ]
) %>%
  mutate(t = 1:90000) %>%
  pivot_longer(cols = -t)

ggplot(gamma, aes(x = t, y = value, color = name)) +
  geom_line(alpha = 0.5, size = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = colores, labels = etiquetas) +
  labs(y = TeX("$\\gamma$")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(), legend.title = element_blank(),
    legend.position = "none"
    ) +
  ggsave("graphs/gamma_traceplot.png", width = 20, height = 15, units = "cm")

ggplot(gamma, aes(x = value)) +
  geom_density(alpha = 0.3, color = viridis(1), fill = viridis(1)) +
  # guides(color = FALSE) +
  labs(x = TeX("$\\gamma$"), y = TeX("$\\pi(\\gamma | data)$")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  ggsave("graphs/gamma_densities.png", width = 14, height = 10, units = "cm")

gamma %>%
  group_by(name) %>%
  mutate(value = cumsum(value) / 1:90000) %>%
  ungroup() %>%
  ggplot(aes(x = t, y = value, color = name)) +
  geom_line(size = 0.7) +
  scale_color_manual(values = colores, labels = etiquetas) +
  scale_x_continuous(labels = scales::comma) +
  labs(y = TeX("$\\hat{\\mu}_{\\gamma}$")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "none") +
  ylim(c(0, 10)) +
  ggsave("graphs/gamma_ergodic_means.png", width = 14, height = 10, units = "cm")


# Frailties ---------------------------------------------------------------

frailties_chain1 <- BGPhazard::BSBSumm(out1, "omega1") %>%
  select(ind = Individual, omega1 = Mean) %>%
  left_join(BGPhazard::BSBSumm(out1, "omega2") %>%
              select(ind = Individual, omega2 = Mean), by = "ind") %>%
  identity()
  # ggplot(aes(x = ind, y = omega1)) +
  # geom_segment(aes(xend = ind, yend = omega2), color = "dodgerblue1", size = 0.7) +
  # geom_point(aes(shape = "omega1"), size = 2, color = "steelblue4") +
  # geom_point(aes(y = omega2, shape = "omega2"), size = 2, color = "steelblue4") +
  # labs(x = "Individuo", y = TeX("$\\omega$")) +
  # scale_x_continuous(breaks = 1:38, labels = 1:38, limits = c(1, 38)) +
  # scale_shape_manual(values = c("omega1" = 10, "omega2" = 13),
  #                    labels = c("omega1" = "Omega 1", "omega2" = "Omega 2")) +
  # theme_bw() +
  # theme(panel.grid.minor = element_blank(), legend.position = "none")

frailties_chain2 <- BGPhazard::BSBSumm(out2, "omega1") %>%
  select(ind = Individual, omega1 = Mean) %>%
  left_join(BGPhazard::BSBSumm(out2, "omega2") %>%
              select(ind = Individual, omega2 = Mean), by = "ind") %>%
  identity()
  # ggplot(aes(x = ind, y = omega1)) +
  # geom_segment(aes(xend = ind, yend = omega2), color = "dodgerblue1") +
  # geom_point(aes(shape = "omega1"), size = 2, color = "steelblue4") +
  # geom_point(aes(y = omega2, shape = "omega2"), size = 2, color = "steelblue4") +
  # labs(x = "Individuo", y = TeX("$\\omega$")) +
  # scale_x_continuous(breaks = 1:38, labels = 1:38, limits = c(1, 38)) +
  # scale_shape_manual(values = c("omega1" = 10, "omega2" = 13),
  #                    labels = c("omega1" = "Omega 1", "omega2" = "Omega 2")) +
  # theme_bw() +
  # theme(panel.grid.minor = element_blank(),
  #       legend.title = element_blank())

frailties_chain3 <- BGPhazard::BSBSumm(out3, "omega1") %>%
  select(ind = Individual, omega1 = Mean) %>%
  left_join(BGPhazard::BSBSumm(out3, "omega2") %>%
              select(ind = Individual, omega2 = Mean), by = "ind") %>%
  identity()
  # ggplot(aes(x = ind, y = omega1)) +
  # geom_segment(aes(xend = ind, yend = omega2), color = "dodgerblue1") +
  # geom_point(aes(shape = "omega1"), size = 2, color = "steelblue4") +
  # geom_point(aes(y = omega2, shape = "omega2"), size = 2, color = "steelblue4") +
  # labs(x = "Individuo", y = TeX("$\\omega$")) +
  # scale_x_continuous(breaks = 1:38, labels = 1:38, limits = c(1, 38)) +
  # scale_shape_manual(values = c("omega1" = 10, "omega2" = 13),
  #                    labels = c("omega1" = "Omega 1", "omega2" = "Omega 2")) +
  # theme_bw() +
  # theme(panel.grid.minor = element_blank(), legend.position = "none")

frailties_chain1 %>%
  bind_rows(
    frailties_chain2, frailties_chain3
  ) %>%
  group_by(ind) %>%
  summarise(across(starts_with("omega"), mean), .groups = "drop") %>%
  ggplot(aes(x = ind, y = omega1)) +
  geom_segment(aes(xend = ind, yend = omega2), color = viridis(3)[[3]]) +
  geom_point(size = 2, color = viridis(3)[[1]]) +
  geom_point(aes(y = omega2), size = 2, color = viridis(3)[[2]]) +
  labs(x = "Individuo", y = TeX("$\\omega$")) +
  scale_x_continuous(breaks = 1:38, labels = 1:38, limits = c(1, 38)) +
  # scale_shape_manual(values = c("omega1" = 10, "omega2" = 13),
  #                    labels = c("omega1" = "Omega 1", "omega2" = "Omega 2")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank()) +
  ggsave("graphs/frailties.png", width = 20, height = 10, units = "cm")

# Omega 1

omega1_chain1 <- out1$omega1 %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(individuo = 1:38) %>%
  pivot_longer(cols = -individuo) %>%
  mutate(name = as.double(str_remove_all(name, "\\."))) %>%
  mutate(chain = "chain1")

omega1_chain2 <- out2$omega1 %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(individuo = 1:38) %>%
  pivot_longer(cols = -individuo) %>%
  mutate(name = as.double(str_remove_all(name, "\\."))) %>%
  mutate(chain = "chain2")

omega1_chain3 <- out3$omega1 %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(individuo = 1:38) %>%
  pivot_longer(cols = -individuo) %>%
  mutate(name = as.double(str_remove_all(name, "\\."))) %>%
  mutate(chain = "chain3")

omega1_chain1 %>%
  bind_rows(omega1_chain2, omega1_chain3) %>%
  ggplot(aes(x = name, y = value, color = chain)) +
  geom_line(alpha = 0.3) +
  scale_color_manual(values = colores, labels = etiquetas) +
  scale_x_continuous(labels = scales::comma) +
  facet_wrap(~individuo, scales = "free_y") +
  labs(y = TeX("$\\omega_1$"), x = "Iteración") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  ggsave("graphs/traceplots_omega1.png",
         width = 30,
         height = 15,
         units = "cm")

omega1_chain1 %>%
  bind_rows(omega1_chain2, omega1_chain3) %>%
  group_by(individuo, chain) %>%
  mutate(value = cumsum(value) / 1:90000) %>%
  ungroup() %>%
  ggplot(aes(x = name, y = value, color = chain)) +
  geom_line(alpha = 0.3, size = 0.7) +
  scale_color_manual(values = colores, labels = etiquetas) +
  scale_x_continuous(labels = scales::comma) +
  facet_wrap(~individuo, scales = "free_y") +
  labs(y = TeX("$\\hat{\\mu}_{\\omega_1}$"), x = "Iteración") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  ggsave("graphs/ergodic_means_omega1.png",
         width = 30,
         height = 15,
         units = "cm")

# Omega 2

omega2_chain1 <- out1$omega2 %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(individuo = 1:38) %>%
  pivot_longer(cols = -individuo) %>%
  mutate(name = as.double(str_remove_all(name, "\\."))) %>%
  mutate(chain = "chain1")

omega2_chain2 <- out2$omega2 %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(individuo = 1:38) %>%
  pivot_longer(cols = -individuo) %>%
  mutate(name = as.double(str_remove_all(name, "\\."))) %>%
  mutate(chain = "chain2")

omega2_chain3 <- out3$omega2 %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(individuo = 1:38) %>%
  pivot_longer(cols = -individuo) %>%
  mutate(name = as.double(str_remove_all(name, "\\."))) %>%
  mutate(chain = "chain3")

omega2_chain1 %>%
  bind_rows(omega2_chain2, omega2_chain3) %>%
  ggplot(aes(x = name, y = value, color = chain)) +
  geom_line(alpha = 0.3) +
  scale_color_manual(values = colores, labels = etiquetas) +
  scale_x_continuous(labels = scales::comma) +
  facet_wrap(~individuo, scales = "free_y") +
  labs(y = TeX("$\\omega_2$"), x = "Iteración") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  ggsave("graphs/traceplots_omega2.png",
         width = 30,
         height = 15,
         units = "cm")

omega2_chain1 %>%
  bind_rows(omega2_chain2, omega2_chain3) %>%
  group_by(individuo, chain) %>%
  mutate(value = cumsum(value) / 1:90000) %>%
  ungroup() %>%
  ggplot(aes(x = name, y = value, color = chain)) +
  geom_line(alpha = 0.3, size = 0.7) +
  scale_color_manual(values = colores, labels = etiquetas) +
  scale_x_continuous(labels = scales::comma) +
  facet_wrap(~individuo, scales = "free_y") +
  labs(y = TeX("$\\hat{\\mu}_{\\omega_1}$"), x = "Iteración") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  ggsave("graphs/ergodic_means_omega2.png",
         width = 30,
         height = 15,
         units = "cm")


# Hazards -----------------------------------------------------------------

tib_lambda1 <-
  as_tibble(t(out1$lambda1)) %>%
  bind_rows(
    as_tibble(t(out2$lambda1)),
    as_tibble(t(out3$lambda1))
  )

tib_lambda2 <-
  as_tibble(t(out1$lambda2)) %>%
  bind_rows(
    as_tibble(t(out2$lambda2)),
    as_tibble(t(out3$lambda2))
  )

means_l1 <- vector(mode = "double", length = NCOL(tib_lambda1))
prob_low_l1 <- vector(mode = "double", length = NCOL(tib_lambda1))
prob_high_l1 <- vector(mode = "double", length = NCOL(tib_lambda1))
means_l2 <- vector(mode = "double", length = NCOL(tib_lambda1))
prob_low_l2 <- vector(mode = "double", length = NCOL(tib_lambda1))
prob_high_l2 <- vector(mode = "double", length = NCOL(tib_lambda1))

for (i in 1:NCOL(tib_lambda1)) {
  means_l1[[i]] <- mean(tib_lambda1[[i]])
  probs <- stats::quantile(tib_lambda1[[i]], probs = c(.025, .975))
  prob_low_l1[[i]] <- probs[[1]]
  prob_high_l1[[i]] <- probs[[2]]
  means_l2[[i]] <- mean(tib_lambda2[[i]])
  probs <- stats::quantile(tib_lambda2[[i]], probs = c(.025, .975))
  prob_low_l2[[i]] <- probs[[1]]
  prob_high_l2[[i]] <- probs[[2]]
}

haz_plot_tib <- tibble(
  interval = 1:NCOL(tib_lambda1),
  media_l1 = means_l1,
  prob_low_l1 = prob_low_l1,
  prob_high_l1 = prob_high_l1,
  media_l2 = means_l2,
  prob_low_l2 = prob_low_l2,
  prob_high_l2 = prob_high_l2
) %>%
  mutate(
    int_start = seq(from = 0, by = 10, length.out = NROW(.)),
    int_end = seq(from = 10, by = 10, length.out = NROW(.))
    )

haz_plot_tib %>%
  ggplot(aes(x = int_start)) +
  geom_segment(
    aes(y = media_l1, xend = int_end, yend = media_l1),
    color = viridis(2)[[2]],
    size = 1
    ) +
  geom_segment(
    aes(y = prob_low_l1, xend = int_end, yend = prob_low_l1),
    color = viridis(2)[[1]],
    size = 0.5,
    lty = 2
  ) +
  geom_segment(
    aes(y = prob_high_l1, xend = int_end, yend = prob_high_l1),
    color = viridis(2)[[1]],
    size = 0.5,
    lty = 2
  ) +
  scale_x_continuous(
    breaks = seq(from = 0,  by = 40, length.out = NROW(haz_plot_tib)/4)
  ) +
  labs(x = "t", y = TeX("$\\lambda_1$")) +
  ylim(c(0, 0.08)) +
  theme_bw() +
  theme(panel.grid = ggplot2::element_blank()) +
  ggsave(
    "graphs/hazard1.png",
    width = 30,
    height = 15,
    units = "cm"
    )

haz_plot_tib %>%
  ggplot(aes(x = int_start)) +
  geom_segment(
    aes(y = media_l2, xend = int_end, yend = media_l2),
    color = viridis(2)[[2]],
    size = 1
  ) +
  geom_segment(
    aes(y = prob_low_l2, xend = int_end, yend = prob_low_l2),
    color = viridis(2)[[1]],
    size = 0.5,
    lty = 2
  ) +
  geom_segment(
    aes(y = prob_high_l2, xend = int_end, yend = prob_high_l2),
    color = viridis(2)[[1]],
    size = 0.5,
    lty = 2
  ) +
  scale_x_continuous(
    breaks = seq(from = 0,  by = 40, length.out = NROW(haz_plot_tib)/4)
  ) +
  labs(x = "t", y = TeX("$\\lambda_2$")) +
  ylim(c(0, 0.08)) +
  theme_bw() +
  theme(panel.grid = ggplot2::element_blank()) +
  ggsave(
    "graphs/hazard2.png",
    width = 30,
    height = 15,
    units = "cm"
  )

png(filename = "graphs/gamma_acf.png",
    width = 14,
    height = 10,
    units = "cm",
    res = 300)

acf(out1$gamma[1, ], lag.max = 1000, main = "")

dev.off()


# Supervivencia -----------------------------------------------------------

source("R/lib/copula.R")

s1_o1 <- BGPhazard::BSBSumm(out1, "s1") %>%
  select(t1 = t, s1 = `S(t)`)

s2_o1 <- BGPhazard::BSBSumm(out1, "s2") %>%
  select(t2 = t, s2 = `S(t)`)

s1_o2 <- BGPhazard::BSBSumm(out2, "s1") %>%
  select(t1 = t, s1 = `S(t)`)

s2_o2 <- BGPhazard::BSBSumm(out2, "s2") %>%
  select(t2 = t, s2 = `S(t)`)

s1_o3 <- BGPhazard::BSBSumm(out3, "s1") %>%
  select(t1 = t, s1 = `S(t)`)

s2_o3 <- BGPhazard::BSBSumm(out3, "s2") %>%
  select(t2 = t, s2 = `S(t)`)

s1 <- bind_rows(s1_o1, s1_o2, s1_o3) %>%
  group_by(t1) %>%
  summarise(across(s1, mean), .groups = "drop") %>%
  mutate(t1 = as.double(t1)) %>%
  arrange(t1) %>%
  .$s1

s2 <- bind_rows(s2_o1, s2_o2, s2_o3) %>%
  group_by(t2) %>%
  summarise(across(s2, mean), .groups = "drop") %>%
  mutate(t2 = as.double(t2)) %>%
  arrange(t2) %>%
  .$s2

omega1 <- c(out1$omega1, out2$omega1, out3$omega1)
omega2 <- c(out1$omega2, out2$omega2, out3$omega2)

# grid <- matrix(nrow = length(s1), ncol = length(s2))
# 
# for (u in 1:length(s1)) {
#   for (v in 1:length(s2)) {
#     grid[u, v] <- copula(s1[u] - 0.00001, s2[v] - 0.00001, omega1, omega2)
#   }
# }

# write_rds(grid, "cache/grid_surv.rds")
grid <- read_rds("cache/grid_surv.rds")

png(
  filename = "graphs/surv_fun.png",
  width = 30,
  height = 30,
  units = "cm",
  res = 300
)

persp(
  as.double(s1_o1$t1),
  as.double(s1_o1$t1),
  grid,
  # phi = 0,
  theta = 120,
  xlab = "t1",
  ylab = "t2",
  zlab = "S(t1, t2)",
  col = viridis::viridis(2)[[2]],
  border = viridis::viridis(2)[[1]]
)

dev.off()

png(
  filename = "graphs/surv_fun_contour.png",
  width = 30,
  height = 30,
  units = "cm",
  res = 300
)

filled.contour(
  as.double(s1_o1$t1),
  as.double(s1_o1$t1),
  grid,
  color.palette = viridis::viridis
)

dev.off()

png(
  filename = "graphs/surv_fun_marginal.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

plot(
  x = seq(from = 0, to = 570, by = 10),
  y = s1,
  type = "l",
  col = viridis(2)[[1]],
  lwd = 2,
  xlab = TeX("t"),
  ylab = TeX("\\hat{S}(t)")
)
lines(
  x = seq(from = 0, to = 570, by = 10),
  y = s2,
  col = viridis(2)[[2]],
  lwd = 2
)

dev.off()

beepr::beep()
