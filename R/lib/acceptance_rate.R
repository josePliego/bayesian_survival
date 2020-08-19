acceptance_rate <- function(.x) {
  len.x <- length(.x)
  
  s <- sum(.x[1:(len.x-1)] == .x[2:len.x])
  
  return(1 - s/(len.x - 1))
}
