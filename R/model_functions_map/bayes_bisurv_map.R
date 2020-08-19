bayes_bisurv <- function(
  dt,
  alpha,
  beta,
  c,
  int.len,
  iter,
  gamma.d,
  burnin = 0,
  seed = 0
){
  
  set.seed(seed)
  
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
  
  x.matrix <- matrix(
    unlist(x),
    nrow = n.obs,
    byrow = TRUE
  )
  
  max.t <- max(max(t1), max(t2))
  
  t.part <- partition(t = max.t, int.len = int.len)
  
  # Variable initialization
  
  rho <- runif(n = 1)
  gamma <- rho / (1 - rho)
  
  omega1 <- rgamma(n = n.obs, shape = 2, rate = 1)
  y <- rpois(n = n.obs, lambda = omega1 )
  omega2 <- rgamma(n = n.obs, shape = 2, rate = 1)
  
  # TODO checar
  theta <- rnorm(n = length(x[[1]]), sd = 0.1)
  
  n.intervals <- length(t.part) - 1
  
  lambda1 <- rgamma(n = n.intervals, shape = alpha, rate = beta)
  lambda2 <- rgamma(n = n.intervals, shape = alpha, rate = beta)
  u1 <- rpois(n = n.intervals, lambda = c * lambda1)
  u2 <- rpois(n = n.intervals, lambda = c * lambda2)
  
  # Outputs
  # omega1.expectation <- rep(0, times = n.obs)
  # omega2.expectation <- rep(0, times = n.obs)
  # lambda1.expectation <- rep(0, times = n.intervals)
  # lambda2.expectation <- rep(0, times = n.intervals)
  # theta.expectation <- rep(0, times = length(theta))
  
  outputs <- list(
    "omega1" = list(),
    "omega2" = list(),
    "lambda1" = list(),
    "lambda2" = list(),
    "t1" = list(),
    "t2" = list(),
    "gamma" = list(),
    "u1" = list(),
    "u2" = list(),
    "y" = list()
  )
  
  if (predictors == 1) {
    outputs$theta <- list()
  }
  
  theta.chain <- theta
  
  t1.current <- t1
  t2.current <- t2
  
  for (i in 1:iter) {
    
    p.bar$tick(1)
    
    cum.h1 <- cum_h(t1.current, t.part, lambda1)
    cum.h2 <- cum_h(t2.current, t.part, lambda2)
    
    part.count1 <- partition_count(t1.current, t.part)
    part.count2 <- partition_count(t2.current, t.part)
    
    part.loc1 <- partition_location(t1.current, t.part)
    part.loc2 <- partition_location(t2.current, t.part)
    
    omega1 <- purrr::pmap_dbl(
      list(omega1, y, cum.h1, x),
      function(x1, x2, x3, x4) {
        sample_omega(x1, x2, x3, x4, theta, gamma)
      }
    )
    
    omega2 <- purrr::pmap_dbl(
      list(omega2, y, cum.h2, x),
      function(x1, x2, x3, x4) {
        sample_omega(x1, x2, x3, x4, theta, gamma)
      }
    )
    
    y <- purrr::map2_dbl(
      omega1, omega2,
      ~ sample_y(.x, .y, gamma)
    )
    
    t.partition.low1 <- purrr::map_dbl(
      part.loc1,
      ~ t.part[.]
    )
    
    t.partition.low2 <- purrr::map_dbl(
      part.loc2,
      ~ t.part[.]
    )
    
    lambda.bounds1 <- purrr::map_dbl(
      1:length(lambda1),
      ~ get_min_bound(
        t1.current,
        omega1,
        x,
        part.loc1,
        t.partition.low1,
        .,
        theta,
        lambda1,
        int.len
      )
    )
    
    lambda.bounds2 <- purrr::map_dbl(
      1:length(lambda2),
      ~ get_min_bound(
        t2.current,
        omega2,
        x,
        part.loc2,
        t.partition.low2,
        .,
        theta,
        lambda2,
        int.len
      )
    )
    
    u1.1 <- u1
    u1.2 <- c(0, u1[1:(length(u1) - 1)])
    
    u2.1 <- u2
    u2.2 <- c(0, u2[1:(length(u2) - 1)])
    
    c1 <- rep(c, times = length(lambda1))
    c2 <- c(0, c1[1:(length(c1) - 1)])
    
    lambda1 <- purrr::pmap_dbl(
      list(u1.1, u1.2, c1, c2, lambda.bounds1, part.count1),
      function(x1, x2, x3, x4, x5, x6) {
        sample_lambda(x1, x2, alpha, beta, x3, x4, x5, x6)
      }
    )
    
    lambda2 <- purrr::pmap_dbl(
      list(u2.1, u2.2, c1, c2, lambda.bounds2, part.count2),
      function(x1, x2, x3, x4, x5, x6) {
        sample_lambda(x1, x2, alpha, beta, x3, x4, x5, x6)
      }
    )
    
    index.indicator <- c(rep(1, times = (length(u1) - 1)), 0)
    
    lambda1.lag <- c(lambda1[2:length(lambda1)], 1)
    lambda2.lag <- c(lambda2[2:length(lambda2)], 1)
    
    u1 <- purrr::pmap_dbl(
      list(lambda1, lambda1.lag, index.indicator),
      function(x1, x2, x3) {
        sample_u(x1, x2, alpha, beta, c, x3)
      }
    )
    
    u2 <- purrr::pmap_dbl(
      list(lambda2, lambda2.lag, index.indicator),
      function(x1, x2, x3) {
        sample_u(x1, x2, alpha, beta, c, x3)
      }
    )
    
    gamma <- sample_gamma(gamma, omega1, omega2, y, gamma.d)
    
    if (predictors == 1) {
      
      theta.bound1 <- map_dbl(
        1:length(theta),
        ~ get_min_bound_theta(., t1.current, omega1, cum.h1, x, theta)
      )
      
      theta.bound2 <- map_dbl(
        1:length(theta),
        ~ get_min_bound_theta(., t2.current, omega2, cum.h2, x, theta)
      )
      
      theta.bound <- pmin(theta.bound1, theta.bound2)
      
      # print(theta.bound)
      
      # print(colSums(x.matrix))
      
      theta <- purrr::pmap_dbl(
        list(theta.bound, colSums(x.matrix), theta),
        function(x1, x2, x3) {sample_theta(x1, x2, x3)}
      )
      
      # print(theta)
      
      outputs$theta[[i]] <- theta
      
    }
    
    t1.current <- purrr::pmap_dbl(
      list(t1, t1.current, omega1, delta1, x),
      function(x1, x2, x3, x4, x5) {
        sample_t(x1, x2, x3, x4, max(t.part), x5, theta, t.part, lambda1)
      }
    )
    
    t2.current <- purrr::pmap_dbl(
      list(t2, t2.current, omega2, delta2, x),
      function(x1, x2, x3, x4, x5) {
        sample_t(x1, x2, x3, x4, max(t.part), x5, theta, t.part, lambda2)
      }
    )
    
    outputs$omega1[[i]] <- omega1
    outputs$omega2[[i]] <- omega2
    outputs$lambda1[[i]] <- lambda1
    outputs$lambda2[[i]] <- lambda2
    outputs$t1[[i]] <- t1.current
    outputs$t2[[i]] <- t2.current
    outputs$gamma[[i]] <- gamma
    outputs$u1[[i]] <- u1
    outputs$u2[[i]] <- u2
    outputs$y[[i]] <- y
    
  }
  
  return(outputs)
  
}