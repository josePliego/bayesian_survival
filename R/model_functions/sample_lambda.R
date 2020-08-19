sample_lambda <- function(
  t,
  omega,
  x,
  theta,
  u,
  lambda,
  alpha,
  beta,
  c,
  t.partition
) {

  part.loc <- partition_location(t, t.partition)
  len.out <- length(lambda)
  
  part.len <- t.partition[2] - t.partition[1]
  part.count <- partition_count(t, t.partition)

  for (i in 1:len.out) {
    l.bound <- 2

    for (j in 1:length(t)) {

      omega.h <- omega[j] * exp(-(x[[j]] %*% theta))

      if (i < part.loc[j]) {
        
        current.bound <- 0

        if (part.loc[j] > 1) {
          for (k in 1:(part.loc[j] - 1)) {
            current.bound <- current.bound +
              (lambda[k] * part.len) * (k != i)
          }
        }

        current.bound <- current.bound +
          lambda[part.loc[j]] * (t[j] - t.partition[part.loc[j]])

        current.bound <- (omega.h - current.bound) / part.len
        
        if (is.na(current.bound)) {
          print(paste("lambda[part.loc[j]] ", lambda[part.loc[j]]))
          print(paste("t[j] ", t[j]))
          print(paste("t.partition[part.loc[j]]", t.partition[part.loc[j]]))
          stop("Bailo Bertha en primer if")
        }

      } else if (i == part.loc[j]) {
        
        current.bound <- 0

        if (part.loc[j] > 1) {
          for (k in 1:(part.loc[j] - 1)) {
            current.bound <- current.bound +
              (lambda[k] * part.len)
          }
        }

        current.bound <- (omega.h - current.bound) / (t[j] - t.partition[part.loc[j]])
        
        if (is.na(current.bound)) {
          stop("Failed")
        }

      } else {

        current.bound <- 2

      }

      l.bound <- min(l.bound, current.bound)

    }

    alpha.l <- ifelse(
      i > 1,
      alpha + u[i] + u[i - 1] + part.count[i],
      alpha + u[i] + part.count[i]
    )

    beta.l <- beta + c + c*(i > 1)
    
    if (l.bound < 1e-6) {l.bound <- 1}

    denominator <- pgamma(l.bound * beta.l, shape = alpha.l, rate = 1)
    
    # print(paste0("alpha.l", alpha.l))
    # print(paste0("l.bound ", l.bound))
    # print(paste0("beta.l ", beta.l))
    # print(paste0("denominator ", denominator))
    
    if (denominator == 0) { denominator <- 0.005 }

    unif <- runif(n = 1)

    f <- function(x) {
      pgamma(x, shape = alpha.l, rate = 1) / denominator - unif
    }

    solution <- uniroot(f, lower = 0, upper = l.bound * beta.l)$root

    lambda.prueba <- solution / beta.l
    if (lambda.prueba < 1e-5) { lambda.prueba <- 1e-5}
    lambda[i] <- lambda.prueba

  }

  return(lambda)

}