# :vim set filetype=R
#context("confine")
assert("Basic behavior is correct", {
  x <- -2.5 : 2.5
  y <- c(-1, -1, -.5, .5, 1, 1)
  all.equal(confine(x), y)
})

assert("The min.level and max.level options work", {
  x <- -4.5 : 4.5
  y <- c(-2, -2, -2, -1.5, -.5, .5, 1.5, 2, 2, 2)
  all.equal(confine(x, max.level=2, min.level=-2), y)
})

assert("Confine fails when min.level >= max.level", {
  x <- 1:100
  y <- rnorm(100, sd=4)

  tryCatch(confine(y, min.len=1, max.len=-1), function(e) TRUE)
  tryCatch(confine(y, min.len=1, max.len=1), function(e) TRUE)
})

assert("Confine fails for character input", {
  x <- 'abcdefghijk'

  tryCatch(confine(x), function(e) TRUE)
  tryCatch(confine(x, min.len='b', max.len='h'), function(e) TRUE)
})


