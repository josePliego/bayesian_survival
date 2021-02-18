alpha <- 2
beta <- 1
gamma <- 5

n.sim <- 1e4

set.seed(42)

lambda <- rgamma(n = n.sim, shape = alpha, rate = beta)
x <- rpois(n = n.sim, lambda = lambda*gamma)

png(
  filename = "graphs/poisson_gamma.png",
  width = 30,
  height = 20,
  units = "cm",
  res = 300
)

hist(
  x,
  breaks = seq(from = 0, to = 70, by = 2),
  probability = TRUE,
  col = viridis::viridis(1),
  border = viridis::viridis(1),
  main = "",
  xlab = "",
  ylab = ""
  )

dev.off()
