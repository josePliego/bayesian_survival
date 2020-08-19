theta_restriction <- function(
  t,
  omega,
  cum.h,
  x,
  theta.index,
  theta
) {
  
  current.theta <- theta[theta.index]
  current.x <- x[theta.index]
  
  if (current.x == 0) {
    bound <- 1e4
    return(bound)
  }
  
  bound <- (log(omega) - log(cum.h) - x %*% theta + current.theta * current.x) / current.x
  
  return(bound)
  
}

get_min_bound_theta <- function(
  theta.index,
  t,
  omega,
  cum.h,
  x,
  theta
) {
  
  bounds <- purrr::pmap_dbl(
    list(t, omega, cum.h, x),
    function(x1, x2, x3, x4) {
      theta_restriction(
        x1,
        x2,
        x3,
        x4,
        theta.index,
        theta
        )
      }
    )
  
  return(min(bounds))
  
  }

sample_theta <- function(
  bound,
  sum.x,
  theta
  ) {
  
  # if (theta >= bound) {
  #   
  #   proposal <- runif(n = 1, min = -bound, max = bound)
  #   
  # } else {
  #   
  #   proposal <- runif(n = 1, min = 2 * theta - bound, max = bound)
  #   
  # }
  
  proposal <- rnorm(n = 1, mean = theta, sd = 0.3)
  
  while (proposal >= abs(bound) | proposal <= -abs(bound)) {
    # print(paste(theta, bound, proposal))
    proposal <- rnorm(n = 1, mean = theta, sd = 0.3)
    # print(paste(proposal, bound))
  }
  
  if (theta > bound) {
    
    out <- proposal
    
    } else {
      
      l.rho <- proposal * (2 * sum.x - 0.5 * proposal) -
        theta * (2 * sum.x - 0.5 * theta)

      alpha <- min(exp(l.rho), 1)
      
      # print(alpha)

      u <- runif(n = 1)

      out <- theta + (proposal - theta) * (u <= alpha)

    }

  return(out)

}
