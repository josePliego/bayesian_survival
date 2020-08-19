# Returns the interval of the partition in which t is located.

partition_location <- function(t, partition) {

  lt <- length(t)
  t_loc <- rep(0, times = lt)

  for (i in 1:lt) {
    j <- 1
    while(t[i] > partition[j + 1] & j < length(partition)) {
      j <- j + 1
    }
    t_loc[i] <- j
  }

  return(t_loc)

}
