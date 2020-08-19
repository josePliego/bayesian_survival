f <- function(x) {dgamma(x, shape = 2, rate = 1)}
g <- function(x) {dexp(x, rate = 0.5)}
h <- function(x) {f(x)/g(x)}

set.seed(42)

n.sim <- 1e5

M <- 4/exp(1)

proposal <- rexp(n = n.sim, rate = 0.5)

u <- runif(n = n.sim)

aceptados <- u <= 1/M * h(proposal)

sample.f <- proposal[aceptados]

png(
  filename = "graphs/aceptacion_rechazo.png",
  width = 30,
  height = 20,
  units = "cm",
  res = 300
  )

hist(
  sample.f,
  probability = TRUE,
  breaks = seq(from = 0, to = max(sample.f) + 0.05, by = 0.05),
  ylim = c(0, 0.6),
  xlim = c(0, 10),
  xlab = "x",
  ylab = "",
  main = "",
  col = "lightsteelblue1",
  border = "lightsteelblue1"
  )

curve(
  f,
  from = 0,
  to = 10,
  add = TRUE,
  lwd = 2,
  col = "steelblue4"
  )

curve(
  M*g(x),
  from = 0,
  to = 10,
  col = "steelblue4",
  lwd = 2,
  add = TRUE,
  lty = 2
  )

dev.off()
