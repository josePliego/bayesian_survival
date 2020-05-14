inicial <- 0
final <- 570
incremento <- 10

particion <- seq(from = inicial, to = final, by = incremento)

alpha <- 1
beta <- 1
c <- 100

lambda <- rgamma(shape = alpha, rate = beta, n = 1)
u <- rpois(lambda = c*lambda, n = 1)

for (i in 1:(length(particion) - 2)) {
  lambda <- c(lambda, rgamma(shape = alpha + u[i], rate = beta + c, n = 1))
  u <- c(u, rpois(lambda = c * lambda[i+1], n = 1))
}

h <- function(t) {
  indicador_intervalo <- c()
  
  for (i in 1:(length(particion) - 1)) {
    indicador_intervalo <-
      c(
        indicador_intervalo,
        (t > particion[i] & t <= particion[i+1])
        )
  }
  
  loc <- which(indicador_intervalo == TRUE)
  
  ht <- lambda[loc]
  
  return(ht)
  
}


h(1)
h(11)
h(21)
h(561)