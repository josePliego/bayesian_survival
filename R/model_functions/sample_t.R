sample_t <- function(
  t.original,
  t.previous,
  omega,
  delta,
  partition,
  lambda,
  x,
  theta
  ) {

  t.out <- vector(mode = "double", length = length(t.original))

  for (i in 1:length(t.out)) {

    bound <- t.original[i]

    if (delta[i] == 1) {
      t.out[i] <- t.previous[i]
      next
    }

    u <- runif(n = 1)

    f <- function(var) {cum_h(var, partition, lambda) * exp(x[[i]] %*% theta) - u*omega[i]}

    up <- max(partition)

    while (f(up) < 0) {
      up <- up + 1
    }

    proposal <- uniroot(
      f,
      lower = 0 ,
      upper = up
      )$root

    t.out[i] <- t.previous[i] +
      (proposal - t.previous[i]) * (proposal > bound & proposal <= max(partition))

    }

  return(t.out)

}