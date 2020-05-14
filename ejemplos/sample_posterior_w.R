sample_posterior_w <- function(gamma, y, Ht, nsim) {
  
  i = 1
  
  sim <- vector(length = nsim, mode = "double")
  
  while(i <= nsim) {
    samp <- rgamma(shape = y + 1, rate = 1 + gamma, n = 1)
    
    if (samp > Ht) {
      sim[i] <- samp
      i = i+1
    }
    
  }
  
  return(sim)
  
}
