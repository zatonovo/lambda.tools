# :vim set filetype=R

#context("partition")
assert("Basic functionality of partition works for a small sequence", {
  x <- 1:5
  y <- matrix(c(1, 3, 5, 7, 5, 7, 9, 5), ncol=2)
  anynames(y) <- c("left", "right")
  all.equal(partition(x, metric=sum, radius=2), y)
})

assert("Partition works for a non-default radius", {
  x <- 1:5
  y <- matrix(c(1, 3, 6, 9, 9, 12, 9, 5), ncol=2)
  anynames(y) <- c("left", "right")
  all.equal(partition(x, metric=sum, radius=3), y)
})

assert("A matrix is not allowed as input", {
  x <- matrix(rnorm(10), ncol=2)
  tryCatch(partition(x), error=function(e) TRUE)
})

