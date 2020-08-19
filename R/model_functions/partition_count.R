# Count how many failure times are in each interval of partition
partition_count <- function(t, partition) {
  t_loc <- partition_location(t, partition)
  counts <- rep(0, times = (length(partition) - 1))

  for (i in 1:length(counts)) {
    counts[i] <- sum(t_loc == i)
  }

  return(counts)

}