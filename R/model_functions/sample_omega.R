sample_omega <- function(
  omega,
  t,
  y,
  x,
  theta,
  gamma,
  partition,
  lambda
  ) {

  bound.h <- cum_h(t, partition, lambda)

  len.out <- length(omega)
  omega.out <- vector(mode = "numeric", length = len.out)

  for (i in 1:len.out) {
    # print(paste("x[i] ", x[i]))
    # print(paste("theta ", theta))
    # print(paste("exp(x[[i]] %*% theta) ", exp(x[[i]] %*% theta)))
    bound <- bound.h[i] * exp(x[[i]] %*% theta)
    proposal <- runif(
      n = 1,
      min = max(bound, omega[i] - y[i] - 1),
      max = omega[i] + y[i] + 1
      )

    # if the previous observation is not greater than the bound,
    # the Metropolis step always accepts

    if (omega[i] <= bound) {

      omega.out[i] <- proposal

    } else {

      log.alpha <- y[i] * (log(proposal) - log(omega[i])) -
        (1 + gamma) * (proposal - omega[i])

      prob <- min(exp(log.alpha), 1)
      u <- runif(n = 1)

      omega.out[i] <- omega[i] + (proposal - omega[i]) * (u <= prob)

    }


  }

  return(omega.out)

}