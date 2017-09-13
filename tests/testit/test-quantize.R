# :vim set filetype=R
#context("quantize")
assert("quantize works for a small sequence and default metric", {
  x <- seq(-2, 2, by=.1)
  y <- c(rep(-1, 16), rep(0, 10), rep(1, 15))
  all.equal(quantize(x), y)
})

assert("quantize works for a small sequence and Euclidean distance metric", {
  x <- seq(-2, 2, by=.1)
  y <- c(rep(-1, 16), rep(0, 10), rep(1, 15))
  all.equal(quantize(x), y)
})

assert("Bin ordering does not affect ties.", {
  x <- -20:20 / 10
  bins <- c(-1, 0, 1)
  y1 <- quantize(x, bins=c(-1, 0, 1))
  y2 <- quantize(x, bins=c(1, 0, -1)) 
  all.equal(y1, y2)
})


