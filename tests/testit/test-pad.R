#context("pad")
assert("pad pads corrrect number of NAs to head", {
  x <- 1:5
  y <- c(rep(NA, 5), 1:5)
  all.equal(pad(x, 5), y)
})

assert("pad pads corrrect number of NAs to tail", {
  x <- 1:5
  y <- c(1:5, rep(NA, 5))
  all.equal(pad(x, 0, 5), y)
})

assert("pad pads corrrect number of NAs to head and tail", {
  x <- 1:5
  y <- c(rep(NA, 5), 1:5, rep(NA, 5))
  all.equal(pad(x, 5, 5), y)
})

assert("pad pads corrrect number of 0s to head and tail", {
  x <- 1:5
  y <- c(rep(0, 5), 1:5, rep(0, 5))
  all.equal(pad(x, 5, 5, default=0), y)
})

assert("x can not be a 2-d array.", {
  x <- matrix(rnorm(10), ncol=2)
  tryCatch(pad(x, 10), error=function(e) TRUE)
})


