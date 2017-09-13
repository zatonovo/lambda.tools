#context("is.scalar")
assert("scalar argument returns true", {
  is.scalar(10)
})

assert("Non-scalar argument returns false.", {
  !is.scalar(c(10, 10))
})


#context("onlyif")
assert("onlyif works with positive condition", {
  x <- 1:5
  y <- onlyif(length(x) < 10, function(y) pad(y, 10 - length(y)), x)
  all.equal(y, c(NA, NA, NA, NA, NA, 1, 2, 3, 4, 5))
})

assert("onlyif works with negative condition", {
  x <- 1:20
  y <- onlyif(length(x) < 10, function(y) fold(x, function(x, y) x+y), x)
  all.equal(y, x)
})

assert("fn argument must be a function.", {
  x <- rnorm(5)
  constant <- 'string' 
  all.equal(onlyif(TRUE, constant, x), "string")
})


#context("use_default")
assert("use_default works", {
  x <- c(1, 2, 3, NA, NA)
  y <- map(x, function(a) use_default(a, 0))
  all.equal(y, c(1, 2, 3, 0, 0))
})

assert("Non-scalar arguement throws an error.", {
  
  tryCatch(use_default(c(1, 1), 0), error=function(e) TRUE)
})
