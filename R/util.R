# Modify from https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
split_vector <- function(x, batch_size) {
  split(x, ceiling(seq_along(x) / batch_size))
}
