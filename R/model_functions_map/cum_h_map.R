cum_h <- function(t, partition, lambda) {

  cumhaz <- vector(mode = "double", length = length(t))

  int.len <- partition[2] - partition[1]

  index <- partition_location(t, partition)

  for (i in 1:length(t)) {
    loc <- index[i]

    if (loc > length(lambda)) {
      loc <- length(lambda)
    }

    cum <- 0
    if (loc > 1) {
      cum <- sum(lambda[1:(loc - 1)]) * int.len
    }
    cumhaz[i] <- cum + (t[i] - partition[loc]) * lambda[loc]
  }

  return(cumhaz)

}