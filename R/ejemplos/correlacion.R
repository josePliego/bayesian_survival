library(latex2exp)

n.sim <- 1e4

set.seed(42)
x1 <- rnorm(n.sim)
u <- runif(n.sim)
x2 <- ifelse(u <= 0.5, x1, -x1)
y <- rnorm(n.sim)

cor(x1, x2)
cor(x1, y)

png(
  filename = "graphs/correlation.png",
  width = 25,
  height = 25,
  units = "cm",
  res = 300
  )

plot(
  x1,
  x2,
  ylim = c(-4, 4),
  xlim = c(-4, 4),
  xlab = TeX("$x_1$"),
  ylab = TeX("$x_2$"),
  pch = 19,
  col = scales::alpha(viridis::viridis(1), 0.3)
)

dev.off()

png(
  filename = "graphs/correlation_ind.png",
  width = 25,
  height = 25,
  units = "cm",
  res = 300
)

plot(
  x1,
  y,
  ylim = c(-4, 4),
  xlim = c(-4, 4),
  xlab = TeX("$y_1$"),
  ylab = TeX("$y_2$"),
  pch = 19,
  col = scales::alpha(viridis::viridis(1), 0.3)
)

dev.off()
