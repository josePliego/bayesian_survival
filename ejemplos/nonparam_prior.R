intervalos <- 50
alpha <- 1
beta <- 1
c <- 100

lambda <- rgamma(shape = alpha, rate = beta, n = 1)
u <- rpois(lambda = c*lambda, n = 1)

for (i in 1:(intervalos-1)) {
  lambda <- c(lambda, rgamma(shape = alpha + u[i], rate = beta + c, n = 1))
  u <- c(u, rpois(lambda = c * lambda[i+1], n = 1))
}

t <- seq(from = 0, to = 1, length.out = 10*intervalos)

h <- c()
for (i in 1:intervalos) {
  h <- c(h, rep(lambda[i], times = 10))
}

plot(t, h, type = "l")
