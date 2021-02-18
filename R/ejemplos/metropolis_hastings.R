library(latex2exp)

nsim <- 1e4
x1 <- vector(mode = "double", length = nsim)
x2 <- vector(mode = "double", length = nsim)
x3 <- vector(mode = "double", length = nsim)

# x1 propuestas independientes
set.seed(42)
x1[1] <- 0

for (i in 1:(nsim - 1)) {
  anterior <- x1[i]
  candidato <- rnorm(n = 1)
  
  rho <- dt(candidato, df = 5) / dt(anterior, df = 5)
  rho <- rho * dnorm(anterior) / dnorm(candidato)
  
  alpha <- min(rho, 1)
  u <- runif(n = 1)
  
  x1[i + 1] <- anterior + (candidato - anterior) * (u <= alpha)
  
}

# x2 caminata aleatoria normal
set.seed(42)
x2[1] <- 0

for (i in 1:(nsim - 1)) {
  anterior <- x2[i]
  candidato <- rnorm(n = 1, mean = anterior)
  
  rho <- dt(candidato, df = 5) / dt(anterior, df = 5)
  
  alpha <- min(rho, 1)
  u <- runif(n = 1)
  
  x2[i + 1] <- anterior + (candidato - anterior) * (u <= alpha)
  
}

# x3 caminata aleatoria uniforme
set.seed(42)
x3[1] <- 0

for (i in 1:(nsim - 1)) {
  anterior <- x3[i]
  candidato <- runif(n = 1, min = anterior - 1, max = anterior + 1)
  
  rho <- dt(candidato, df = 5) / dt(anterior, df = 5)
  
  alpha <- min(rho, 1)
  u <- runif(n = 1)
  
  x3[i + 1] <- anterior + (candidato - anterior) * (u <= alpha)
  
}

png(
  filename = "graphs/mh_hist_x1.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

hist(
  x1,
  breaks = seq(from = -3.8, to = 3.8, by = 0.1),
  probability = TRUE,
  main = "",
  xlab = "",
  ylab = "",
  col = viridis::viridis(2)[[2]],
  border = viridis::viridis(2)[[2]]
)

curve(
  dt(x, df = 5),
  add = TRUE,
  col = viridis::viridis(2)[[1]]
)

dev.off()

png(
  filename = "graphs/mh_chain_x1.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

plot(
  x1,
  type = "l",
  col = viridis::viridis(1),
  ylab = TeX("$x_1^{(t)}"),
  xlab = "t"
  )

dev.off()

png(
  filename = "graphs/mh_hist_x2.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

hist(
  x2,
  breaks = seq(from = -7.3, to = 7.3, by = 0.1),
  probability = TRUE,
  main = "",
  xlab = "",
  ylab = "",
  col = viridis::viridis(2)[[2]],
  border = viridis::viridis(2)[[2]]
)

curve(
  dt(x, df = 5),
  add = TRUE,
  col = viridis::viridis(2)[[1]]
)

dev.off()

png(
  filename = "graphs/mh_chain_x2.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

plot(
  x2,
  type = "l",
  col = viridis::viridis(1),
  ylab = TeX("$x_2^{(t)}"),
  xlab = "t"
)

dev.off()

png(
  filename = "graphs/mh_hist_x3.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

hist(
  x3,
  breaks = seq(from = -6, to = 6, by = 0.1),
  probability = TRUE,
  main = "",
  xlab = "",
  ylab = "",
  col = viridis::viridis(2)[[2]],
  border = viridis::viridis(2)[[2]]
)

curve(
  dt(x, df = 5),
  add = TRUE,
  col = viridis::viridis(2)[[1]]
)

dev.off()

png(
  filename = "graphs/mh_chain_x3.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

plot(
  x3,
  type = "l",
  col = viridis::viridis(1),
  ylab = TeX("$x_3^{(t)}"),
  xlab = "t"
)

dev.off()
