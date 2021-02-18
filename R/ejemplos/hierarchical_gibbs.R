library(latex2exp)

set.seed(42)
nsim <- 1e4

alpha <- 2
beta <- 4
lambda <- 16

omega <- vector(mode = "double", length = nsim)
gamma <- vector(mode = "double", length = nsim)
theta <- vector(mode = "double", length = nsim)

omega[1] <- rbinom(n = 1, size = 5, prob = 0.5)
theta[1] <- rbeta(n = 1, shape1 = alpha, shape2 = beta)
gamma[1] <- rpois(n = 1, lambda = 4) + omega[1]

for (i in 2:nsim) {
  omega[i] <- rbinom(n = 1, size = gamma[i - 1], prob = theta[i - 1])
  theta[i] <- rbeta(
    n = 1,
    shape1 = omega[i] + alpha,
    shape2 = gamma[i - 1] - omega[i] + beta
    )
  gamma[i] <- omega[i] + rpois(n = 1, lambda = lambda * (1 - theta[i]))
}

png(
  filename = "graphs/hier_hist_omega.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

hist(
  omega,
  breaks = seq(from = -1, to = 26, by = 1),
  border = viridis::viridis(1),
  col = viridis::viridis(1),
  probability = TRUE,
  main = "",
  xlab = "",
  ylab = ""
  )

dev.off()

png(
  filename = "graphs/hier_hist_theta.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

hist(
  theta,
  breaks = seq(from = 0, to = 1, by = 0.01),
  border = viridis::viridis(1),
  col = viridis::viridis(1),
  ylab = "",
  xlab = "",
  main = "",
  probability = TRUE
  )

dev.off()

png(
  filename = "graphs/hier_hist_gamma.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

hist(
  gamma,
  breaks = seq(from = 3, to = 33, by = 1),
  border = viridis::viridis(1),
  col = viridis::viridis(1),
  ylab = "",
  xlab = "",
  main = "",
  probability = TRUE
  )

dev.off()

png(
  filename = "graphs/hier_chain_omega.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

plot(
  omega,
  type = "l",
  col = viridis::viridis(1),
  ylab = TeX("$\\omega^{(t)}$"),
  xlab = "t",
  main = ""
  )

dev.off()

png(
  filename = "graphs/hier_chain_gamma.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

plot(
  gamma,
  type = "l",
  col = viridis::viridis(1),
  ylab = TeX("$\\gamma^{(t)}$"),
  xlab = "t",
  main = ""
)

dev.off()

png(
  filename = "graphs/hier_chain_theta.png",
  width = 25,
  height = 20,
  units = "cm",
  res = 300
)

plot(
  theta,
  type = "l",
  col = viridis::viridis(1),
  ylab = TeX("$\\theta^{(t)}$"),
  xlab = "t",
  main = ""
)

dev.off()
