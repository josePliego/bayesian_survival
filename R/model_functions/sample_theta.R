theta_bound <- function(
  t,
  omega,
  x,
  theta,
  partition,
  lambda,
  th.index
  ) {

  cum.h <- cum_h(t, partition, lambda)
  current.theta <- theta[th.index]

  bound <- 1e4

  for (i in 1:length(t)) {

    current.x <- as.double(x[[i]][th.index])

    if (current.x == 0) {

      current.bound <- 1e4

    } else {

      current.bound <-
        (log(omega[i]) - log(cum.h[i]) -
           as.double(x[[i]] %*% theta) + current.theta * current.x) / current.x
      
      # print(paste("Hj ", cum.h[i]))
      # print(paste("omega ", omega[i]))
      # print(paste("omega > H*e ", omega[i] > cum.h[i] * exp(x[[i]] %*% theta)))

    }

    bound <- min(bound, current.bound)
    # bound <- 10

  }

  return(bound)

}

sample_theta <- function(
  t1,
  t2,
  omega1,
  omega2,
  x,
  theta,
  partition1,
  partition2,
  lambda1,
  lambda2
) {

  len.out <- length(theta)

  for (i in 1:len.out) {
    b1 <- theta_bound(t1, omega1, x, theta, partition1, lambda1, i)
    b2 <- theta_bound(t2, omega2, x, theta, partition2, lambda2, i)

    sum.x <- 0

    for (j in 1:length(t1)) {
      sum.x <- sum.x + as.double(x[[j]][i])
    }

    bound <- min(b1, b2)
    # print(paste("bound ", bound))

    proposal <- rnorm(n = 1, mean = theta[i], sd = 1)

    while (proposal > bound) {
      proposal <- rnorm(n = 1, mean = theta[i], sd = 1)
    }

    if (theta[i] > bound) {

      theta[i] <- proposal

    } else {

      rho <- 2 * sum.x * (proposal - theta[i]) + theta[i] ** 2 -
        proposal * theta[i]

      alpha <- min(exp(rho), 1)

      u <- runif(n = 1)

      theta[i] <- theta[i] + (proposal - theta[i]) * (u <= alpha)

    }

  }

  return(theta)

}