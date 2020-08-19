sample_t <- function(
  t.original,
  t.previous,
  omega,
  delta,
  max.partition,
  x,
  theta,
  partition,
  lambda
) {
  
  bound <- t.original
  
  if (delta == 1) {
    
    return(t.original)
    
  }
  
  u <- runif(n = 1)
  
  f <- function(var) {
    cum_h(var, partition, lambda) * exp(x %*% theta) - u * omega
  }
  
  up <- max.partition
  
  while (f(up) < 0) {
    # print(f(up))
    up <- up + 10
  }
  
  proposal <- uniroot(f, lower = 0 , upper = up)$root
  
  t.out <- t.previous +
    (proposal - t.previous) * (proposal > bound & proposal <= max.partition)
  
  return(t.out)
    
}
