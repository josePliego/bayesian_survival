# Hazard rate function.
# In: t - time point
#     partition - time interval partition
#     lambda - vector of hazard rates by interval

h <- function(t, partition, lambda) {

  index <- partition_location(t, partition)

  return(lambda[index])

}