library(BSBHaz)
library(tidyverse)
library(latex2exp)
library(patchwork)

bsb_init1 <- bsbhaz_initial_setup(KIDNEY, part_len = 10, seed = 42)
out1 <- bsbhaz_sample(
  bsb_init1,
  iter = 50000,
  burn_in = 5000,
  gamma_d = 50, 
  theta_d = 0.3, 
  seed = 42
)
bsb_init2 <- bsbhaz_initial_setup(KIDNEY, part_len = 10, seed = 43)
out2 <- bsbhaz_sample(
  bsb_init2,
  iter = 50000,
  burn_in = 5000,
  gamma_d = 50, 
  theta_d = 0.3, 
  seed = 43
)
bsb_init3 <- bsbhaz_initial_setup(KIDNEY, part_len = 10, seed = 44)
out3 <- bsbhaz_sample(
  bsb_init3,
  iter = 50000,
  burn_in = 5000,
  gamma_d = 50, 
  theta_d = 0.3, 
  seed = 44
)
bsb_init4 <- bsbhaz_initial_setup(KIDNEY, part_len = 10, seed = 45)
out4 <- bsbhaz_sample(
  bsb_init4,
  iter = 50000,
  burn_in = 5000,
  gamma_d = 50, 
  theta_d = 0.3, 
  seed = 45
)


# Theta -------------------------------------------------------------------

colores <- c("chain1" = "deepskyblue1", "chain2" = "dodgerblue1",
             "chain3" = "steelblue1", "chain4" = "lightskyblue")
etiquetas <- c("chain1" = "Cadena 1", "chain2" = "Cadena 2",
               "chain3" = "Cadena 3", "chain4" = "Cadena 4")
theta <- tibble(
  chain1 = out1$theta[1, ],
  chain2 = out2$theta[1, ],
  chain3 = out3$theta[1, ],
  chain4 = out4$theta[1, ]
  ) %>%
  mutate(t = 1:45000) %>%
  pivot_longer(cols = -t)

ggplot(theta, aes(x = t, y = value, color = name)) +
  geom_line(alpha = 0.7, size = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = colores, labels = etiquetas) +
  labs(y = TeX("$\\theta$")) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.title = element_blank()) +
  ggsave("graphs/theta_traceplot.png", width = 20, height = 15, units = "cm")

ggplot(theta, aes(x = value, color = name)) +
  geom_density(aes(fill = name), alpha = 0.3) +
  scale_fill_manual(values = colores, labels = etiquetas) +
  scale_color_manual(values = colores) +
  guides(color = FALSE) +
  labs(x = TeX("$\\theta$"), y = TeX("$\\pi(\\theta | data)$")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank()) +
  ggsave("graphs/theta_densities.png", width = 14, height = 10, units = "cm")

theta %>%
  group_by(name) %>%
  mutate(value = cumsum(value) / 1:45000) %>%
  ungroup() %>%
  ggplot(aes(x = t, y = value, color = name)) +
  geom_line(size = 0.7) +
  scale_color_manual(values = colores, labels = etiquetas) +
  scale_x_continuous(labels = scales::comma) +
  labs(y = TeX("$\\hat{\\mu}_{\\theta}$")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank()) +
  ggsave("graphs/theta_ergodic_means.png", width = 14, height = 10, units = "cm")


# Gamma -------------------------------------------------------------------

colores <- c("chain1" = "deepskyblue1", "chain2" = "dodgerblue1",
             "chain3" = "steelblue1", "chain4" = "lightskyblue")
etiquetas <- c("chain1" = "Cadena 1", "chain2" = "Cadena 2",
               "chain3" = "Cadena 3", "chain4" = "Cadena 4")
gamma <- tibble(
  chain1 = out1$gamma[1, ],
  chain2 = out2$gamma[1, ],
  chain3 = out3$gamma[1, ],
  chain4 = out4$gamma[1, ]
) %>%
  mutate(t = 1:45000) %>%
  pivot_longer(cols = -t)

ggplot(gamma, aes(x = t, y = value, color = name)) +
  geom_line(alpha = 0.7, size = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = colores, labels = etiquetas) +
  labs(y = TeX("$\\gamma$")) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.title = element_blank()) +
  ggsave("graphs/gamma_traceplot.png", width = 20, height = 15, units = "cm")

ggplot(gamma, aes(x = value, color = name)) +
  geom_density(aes(fill = name), alpha = 0.3) +
  scale_fill_manual(values = colores, labels = etiquetas) +
  scale_color_manual(values = colores) +
  guides(color = FALSE) +
  labs(x = TeX("$\\gamma$"), y = TeX("$\\pi(\\gamma | data)$")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  ggsave("graphs/gamma_densities.png", width = 14, height = 10, units = "cm")

gamma %>%
  group_by(name) %>%
  mutate(value = cumsum(value) / 1:45000) %>%
  ungroup() %>%
  ggplot(aes(x = t, y = value, color = name)) +
  geom_line(size = 0.7) +
  scale_color_manual(values = colores, labels = etiquetas) +
  scale_x_continuous(labels = scales::comma) +
  labs(y = TeX("$\\hat{\\mu}_{\\gamma}$")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank()) +
  ggsave("graphs/gamma_ergodic_means.png", width = 14, height = 10, units = "cm")


# Frailties ---------------------------------------------------------------

frailties_chain1 <- bsbhaz_get_summaries(out1, "omega1") %>%
  select(ind = Individual, omega1 = Mean) %>%
  left_join(bsbhaz_get_summaries(out1, "omega2") %>%
              select(ind = Individual, omega2 = Mean), by = "ind") %>%
  ggplot(aes(x = ind, y = omega1)) +
  geom_segment(aes(xend = ind, yend = omega2), color = "dodgerblue1", size = 0.7) +
  geom_point(aes(shape = "omega1"), size = 2, color = "steelblue4") +
  geom_point(aes(y = omega2, shape = "omega2"), size = 2, color = "steelblue4") +
  labs(x = "Individuo", y = TeX("$\\omega$")) +
  scale_x_continuous(breaks = 1:38, labels = 1:38, limits = c(1, 38)) +
  scale_shape_manual(values = c("omega1" = 10, "omega2" = 13),
                     labels = c("omega1" = "Omega 1", "omega2" = "Omega 2")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "none")

frailties_chain2 <- bsbhaz_get_summaries(out2, "omega1") %>%
  select(ind = Individual, omega1 = Mean) %>%
  left_join(bsbhaz_get_summaries(out1, "omega2") %>%
              select(ind = Individual, omega2 = Mean), by = "ind") %>%
  ggplot(aes(x = ind, y = omega1)) +
  geom_segment(aes(xend = ind, yend = omega2), color = "dodgerblue1") +
  geom_point(aes(shape = "omega1"), size = 2, color = "steelblue4") +
  geom_point(aes(y = omega2, shape = "omega2"), size = 2, color = "steelblue4") +
  labs(x = "Individuo", y = TeX("$\\omega$")) +
  scale_x_continuous(breaks = 1:38, labels = 1:38, limits = c(1, 38)) +
  scale_shape_manual(values = c("omega1" = 10, "omega2" = 13),
                     labels = c("omega1" = "Omega 1", "omega2" = "Omega 2")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank())

frailties_chain3 <- bsbhaz_get_summaries(out3, "omega1") %>%
  select(ind = Individual, omega1 = Mean) %>%
  left_join(bsbhaz_get_summaries(out1, "omega2") %>%
              select(ind = Individual, omega2 = Mean), by = "ind") %>%
  ggplot(aes(x = ind, y = omega1)) +
  geom_segment(aes(xend = ind, yend = omega2), color = "dodgerblue1") +
  geom_point(aes(shape = "omega1"), size = 2, color = "steelblue4") +
  geom_point(aes(y = omega2, shape = "omega2"), size = 2, color = "steelblue4") +
  labs(x = "Individuo", y = TeX("$\\omega$")) +
  scale_x_continuous(breaks = 1:38, labels = 1:38, limits = c(1, 38)) +
  scale_shape_manual(values = c("omega1" = 10, "omega2" = 13),
                     labels = c("omega1" = "Omega 1", "omega2" = "Omega 2")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "none")

frailties_chain4 <- bsbhaz_get_summaries(out2, "omega1") %>%
  select(ind = Individual, omega1 = Mean) %>%
  left_join(bsbhaz_get_summaries(out1, "omega2") %>%
              select(ind = Individual, omega2 = Mean), by = "ind") %>%
  ggplot(aes(x = ind, y = omega1)) +
  geom_segment(aes(xend = ind, yend = omega2), color = "dodgerblue1") +
  geom_point(aes(shape = "omega1"), size = 2, color = "steelblue4") +
  geom_point(aes(y = omega2, shape = "omega2"), size = 2, color = "steelblue4") +
  labs(x = "Individuo", y = TeX("$\\omega$")) +
  scale_x_continuous(breaks = 1:38, labels = 1:38, limits = c(1, 38)) +
  scale_shape_manual(values = c("omega1" = 10, "omega2" = 13),
                     labels = c("omega1" = "Omega 1", "omega2" = "Omega 2")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank())

(frailties_chain1 | frailties_chain2) / (frailties_chain3 | frailties_chain4)

ggsave("graphs/frailties.png", width = 35, height = 15, units = "cm")

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

omega1_chain4 <- out4$omega1 %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(individuo = 1:38) %>%
  pivot_longer(cols = -individuo) %>%
  mutate(name = as.double(str_remove_all(name, "\\."))) %>%
  mutate(chain = "chain4")

omega1_chain1 %>%
  bind_rows(omega1_chain2, omega1_chain3, omega1_chain4) %>%
  ggplot(aes(x = name, y = value, color = chain)) +
  geom_line(alpha = 0.3) +
  scale_color_manual(values = colores) +
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
  bind_rows(omega1_chain2, omega1_chain3, omega1_chain4) %>%
  group_by(individuo, chain) %>%
  mutate(value = cumsum(value) / 1:45000) %>%
  ungroup() %>%
  ggplot(aes(x = name, y = value, color = chain)) +
  geom_line(alpha = 0.3, size = 0.7) +
  scale_color_manual(values = colores) +
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

omega2_chain4 <- out4$omega2 %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(individuo = 1:38) %>%
  pivot_longer(cols = -individuo) %>%
  mutate(name = as.double(str_remove_all(name, "\\."))) %>%
  mutate(chain = "chain4")

omega2_chain1 %>%
  bind_rows(omega2_chain2, omega2_chain3, omega2_chain4) %>%
  ggplot(aes(x = name, y = value, color = chain)) +
  geom_line(alpha = 0.3) +
  scale_color_manual(values = colores) +
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
  bind_rows(omega2_chain2, omega2_chain3, omega2_chain4) %>%
  group_by(individuo, chain) %>%
  mutate(value = cumsum(value) / 1:45000) %>%
  ungroup() %>%
  ggplot(aes(x = name, y = value, color = chain)) +
  geom_line(alpha = 0.3, size = 0.7) +
  scale_color_manual(values = colores) +
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

bsbhaz_get_summaries(out1, "lambda1") %>%
  select(int = Interval, mean = Mean, low = `Prob. Low 95%`, high = `Prob. High 95%`) %>%
  mutate(int_start = seq(from = 0,  to = 560, by = 10),
         int_end = seq(from = 10, to = 570, by = 10)) %>%
  ggplot(aes(x = int_start)) +
  geom_segment(aes(y = mean, xend = int_end, yend = mean),
               color = "steelblue1",
               size = 0.7) +
  geom_segment(aes(y = low, xend = int_end, yend = low),
               lty = 3,
               color = "steelblue1",
               size = 0.7) +
  geom_segment(aes(y = high, xend = int_end, yend = high),
               lty = 3,
               color = "steelblue1",
               size = 0.7) +
  scale_x_continuous(breaks = seq(from = 0, to = 570, by = 30)) +
  scale_y_continuous(breaks = seq(from = 0, to = .25, by = .05)) +
  labs(x = "t", y = TeX("$\\hat{\\mu}_{\\lambda_1}$")) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggsave("graphs/hazard1.png",
         width = 14,
         height = 10,
         units = "cm")

bsbhaz_get_summaries(out1, "lambda2") %>%
  select(int = Interval, mean = Mean, low = `Prob. Low 95%`, high = `Prob. High 95%`) %>%
  mutate(int_start = seq(from = 0,  to = 560, by = 10),
         int_end = seq(from = 10, to = 570, by = 10)) %>%
  ggplot(aes(x = int_start)) +
  geom_segment(aes(y = mean, xend = int_end, yend = mean),
               color = "steelblue1",
               size = 0.7) +
  geom_segment(aes(y = low, xend = int_end, yend = low),
               lty = 3,
               color = "steelblue1",
               size = 0.7) +
  geom_segment(aes(y = high, xend = int_end, yend = high),
               lty = 3,
               color = "steelblue1",
               size = 0.7) +
  scale_x_continuous(breaks = seq(from = 0, to = 570, by = 30)) +
  scale_y_continuous(breaks = seq(from = 0, to = .25, by = .05)) +
  labs(x = "t", y = TeX("$\\hat{\\mu}_{\\lambda_2}$")) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggsave("graphs/hazard2.png",
         width = 14,
         height = 10,
         units = "cm")

png(filename = "graphs/theta_acf.png",
    width = 14,
    height = 10,
    units = "cm",
    res = 300)

acf(out1$theta[1, ], lag.max = 1000, main = "")

dev.off()