sample_omega <- function(omega, y, cum.h, x, theta, gamma) {
  
  bound <- cum.h * exp(x %*% theta)
  
  l1 <- max(bound, omega - y - 1)
  l2 <- omega + y + 1
  
  proposal <- runif(n = 1, min = min(l1, l2), max = max(l1, l2))
  
  if (omega <= bound) {
    
    omega.out <- proposal
    
  } else {
    
    log.alpha <- y * (log(proposal) - log(omega)) -
      (1 + gamma) * (proposal - omega)
    
    prob <- min(exp(log.alpha), 1)
    u <- runif(n = 1)
    
    omega.out <- omega + (proposal - omega) * (u <= prob)
    
  }
  
  return(omega.out)
  
}