bayes_bisurv <- function(
  dt,
  alpha,
  beta,
  c,
  int.len,
  iter,
  burnin = 0
  ){
  
  p.bar <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent)",
    total = iter
    )
  
  p.bar$tick(0)

  dt <- data.frame(dt)

  t1 <- dt$t1
  t2 <- dt$t2
  delta1 <- dt$delta1
  delta2 <- dt$delta2

  dt$t1 <- NULL
  dt$t2 <- NULL
  dt$delta1 <- NULL
  dt$delta2 <- NULL

  n.obs <- length(t1)
  
  predictors <- 0
  
  x <- list()

  if (ncol(dt) > 0) {
    
    predictors <- 1

    for (i in 1:nrow(dt)) {
      pred.vec <- c()
      for (j in 1:ncol(dt)) {
        pred.vec <- c(pred.vec, dt[i, j])
      }
      x[[i]] <- pred.vec
    }

  } else {

    for (i in 1:nrow(dt)) {
      x[[i]] <- c(0)
    }

  }

  max.t <- max(max(t1), max(t2))

  t.part <- partition(t = max.t, int.len = int.len)

  # Variable initialization

  rho <- runif(n = 1)
  gamma <- rho / (1 - rho)

  omega1 <- rgamma(n = n.obs, shape = 2, rate = 1)
  y <- rpois(n = n.obs, lambda = omega1 )
  omega2 <- rgamma(n = n.obs, shape = 2, rate = 1)

  theta <- rnorm(n = length(x[[1]]))

  n.intervals <- length(t.part) - 1

  lambda1 <- rgamma(n = n.intervals, shape = alpha, rate = beta)
  lambda2 <- rgamma(n = n.intervals, shape = alpha, rate = beta)
  u1 <- rpois(n = n.intervals, lambda = c * lambda1)
  u2 <- rpois(n = n.intervals, lambda = c * lambda2)

  # Outputs
  omega1.expectation <- rep(0, times = n.obs)
  omega2.expectation <- rep(0, times = n.obs)
  lambda1.expectation <- rep(0, times = n.intervals)
  lambda2.expectation <- rep(0, times = n.intervals)
  theta.expectation <- rep(0, times = length(theta))
  
  theta.chain <- theta

  t1.current <- t1
  t2.current <- t2

  for (i in 1:iter) {
    
    p.bar$tick(1)

    omega1 <- sample_omega(
      omega1,
      t1.current,
      y,
      x,
      theta,
      gamma,
      t.part,
      lambda1
      )

    omega2 <- sample_omega(
      omega2,
      t2.current,
      y,
      x,
      theta,
      gamma,
      t.part,
      lambda2
    )

    y <- sample_y(omega1, omega2, gamma)

    lambda1 <- sample_lambda(
      t1.current,
      omega1,
      x,
      theta,
      u1,
      lambda1,
      alpha,
      beta,
      c,
      t.part
    )

    lambda2 <- sample_lambda(
      t2.current,
      omega2,
      x,
      theta,
      u2,
      lambda2,
      alpha,
      beta,
      c,
      t.part
    )

    u1 <- sample_u(lambda1, alpha, beta, c)

    u2 <- sample_u(lambda2, alpha, beta, c)

    gamma <- sample_gamma(gamma, omega1, omega2, y)

    t1.current <- sample_t(
      t1,
      t1.current,
      omega1,
      delta1,
      t.part,
      lambda1,
      x,
      theta
    )
    
    t2.current <- sample_t(
      t2,
      t2.current,
      omega2,
      delta2,
      t.part,
      lambda2,
      x,
      theta
    )
    
    if (predictors == 1) {
      
      theta <- sample_theta(
        t1.current,
        t2.current,
        omega1,
        omega2,
        x,
        theta,
        t.part,
        t.part,
        lambda1,
        lambda2
      )
      
    }

  }

  return("DONE")

}
