lambda_restriction <- function(
  t,
  omega,
  x,
  part.loc,
  t.partition.low,
  lambda.index,
  theta,
  lambda,
  part.len
  ) {
  
  omega.h <- omega * exp(-(x %*% theta))
  
  if (lambda.index < part.loc) {
    
    current.bound <- 0
    
    if (part.loc > 1) {
      for (k in 1:(part.loc - 1)) {
        current.bound <- current.bound +
          (lambda[k] * part.len) * (k != lambda.index)
      }
    }
    
    current.bound <- current.bound +
      lambda[part.loc] * (t - t.partition.low)
    
    current.bound <- (omega.h - current.bound) / part.len
    
    if (is.na(current.bound)) {
      stop("FAILED")
    }
    
  } else if (lambda.index == part.loc) {
    
    current.bound <- 0
    
    if (part.loc > 1) {
      for (k in 1:(part.loc - 1)) {
        current.bound <- current.bound +
          (lambda[k] * part.len)
      }
    }
    
    current.bound <- (omega.h - current.bound) / (t - t.partition.low)
    
    if (is.na(current.bound)) {
      stop("FAILED")
    }
    
  } else {
    
    current.bound <- 2
    
  }
  
  return(current.bound)
  
}

get_min_bound <- function(
  t,
  omega,
  x,
  part.loc,
  t.partition.low,
  lambda.index,
  theta,
  lambda,
  part.len
  ) {
  
  bounds <- purrr::pmap_dbl(
    list(t, omega, x, part.loc, t.partition.low),
    function(x1, x2, x3, x4, x5) {
      lambda_restriction(
        x1,
        x2,
        x3,
        x4,
        x5,
        lambda.index,
        theta,
        lambda,
        part.len
      )
    }
    )
  
  return(min(bounds))
  
}

sample_lambda <- function(u1, u2, alpha, beta, c1, c2, min.bound, part.count) {
  # u2 <- 0 si es el primer lambda
  # c2 <- c1 si es despues de la primera lambda
  alpha.l <- alpha + u1 + u2 + part.count
  
  beta.l <- beta + c1 + c2
  
  if (min.bound < 1e-6) {min.bound <- 1}
  
  denominator <- pgamma(min.bound * beta.l, shape = alpha.l, rate = 1)
  
  if (denominator == 0) { denominator <- 0.005 }
  
  unif <- runif(n = 1)
  
  f <- function(x) {
    pgamma(x, shape = alpha.l, rate = 1) / denominator - unif
  }
  
  solution <- uniroot(f, lower = 0, upper = min.bound * beta.l)$root
  
  lambda.prueba <- solution / beta.l
  if (lambda.prueba < 1e-5) { lambda.prueba <- 1e-5}
  lambda <- lambda.prueba
  
  return(lambda)
  
}
