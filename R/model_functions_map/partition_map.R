# Create the time interval partition
partition <- function(t, int.len) {

  last <- ifelse(max(t) %% int.len == 0, max(t), max(t) + int.len)
  part <- seq(from = 0, to = last, by = int.len)

  return(part)

}