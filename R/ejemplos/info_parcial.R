nsim <- 10000
lambda <- 0.1

set.seed(42)

time.samples <- rexp(n = nsim, rate = lambda)

# Suponemos que el monitoreo termino en T = 15

time.censored <- ifelse(time.samples > 15, 15, time.samples)
delta <- ifelse(time.samples > 15, 0, 1)

log_lik <- function(lambda) {
  sum(delta) * log(lambda) - lambda * sum(time.censored)
}

log_lik_exact <- function(lambda) {
  length(time.censored) * log(lambda) - lambda * sum(time.censored)
}

time.drop <- time.censored[delta == 1]

log_lik_drop <- function(lambda) {
  length(time.drop) * log(lambda) - lambda * sum(time.drop)
}

max.ver1 <- sum(delta) / sum(time.censored)
max.ver2 <- 1 / mean(time.censored)
max.ver3 <- 1 / mean(time.drop)

ymin <- log_lik(0.01)

png(filename = "graphs/likelihood.png",
    width = 25,
    height = 20,
    units = "cm",
    res = 300)

curve(
  log_lik,
  from = 0.01,
  to = 0.3,
  ylim = c(ymin, log_lik_drop(max.ver3)),
  xlab = expression(lambda),
  ylab = "",
  yaxt = 'n',
  col = "steelblue"
  )
curve(log_lik_exact, from = 0.01, to = 0.3, add = TRUE, col = "forestgreen")
curve(log_lik_drop, from = 0.01, to = 0.3, add = TRUE, col = "darkorange")
lines(x = c(max.ver1, max.ver1), y = c(ymin, log_lik(max.ver1)), lty = 2, col = "steelblue")
lines(x = c(max.ver2, max.ver2), y = c(ymin, log_lik_exact(max.ver2)), lty = 2, col = "forestgreen")
lines(x = c(max.ver3, max.ver3), y = c(ymin, log_lik_drop(max.ver3)), lty = 2, col = "darkorange")
abline(
  v = 0.1,
  col = "gray80",
  lty = 2)

dev.off()
