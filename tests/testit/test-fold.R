# :vim set filetype=R

#context("1D fold")
assert("a numeric vector is valid", {
  x <- 1:10
  all.equal(fold(x, function(a,b) a+b,0), 55) 
})

assert("an error is thrown when a non-function is used", {
  x <- 1:10
  fn <- "I am not a function"
  tryCatch(fold(x, fn, 0), error=function(e) TRUE) 
})

assert("a vector of length 1 is valid", {
  x <- 1
  all.equal(fold(x, function(a,b) a+b,0), 1)
})

assert("ellipsis is valid", {
  x <- 1:5
  fn <- function(a,acc) {acc[a] <- a^2; acc}
  o <- fold(x, fn, list(), simplify=FALSE)

  all.equal(class(o), 'list')
  all.equal(length(o), 5)
  all.equal(o[[2]], 4)
})


#context("2D fold")
assert("fold with x as a matrix", {
  x <- matrix(1:10, ncol=2)
  y <- fold(x, function(a,b) a+b, 0)
  all.equal(y, c(7, 9, 11, 13, 15))
})

assert("fold with x as a data.frame", {
  x <- data.frame(x1=1:10, x2=1:10) 
  y <- fold(x, function(a,b) a+b, 0)
  all.equal(y, c(2 ,4, 6, 8, 10, 12, 14, 16, 18, 20))
})


#context("1D foldrange")
assert("foldrange with window size not a multiple of length(x)",{
  x <- 1:10
  y <- foldrange(x, 3, function(a,b) a+b)
  all.equal(y, c(36, 44, 52))
})

assert("foldrange with x a vector and with window size > length(x)",{
  x <- 1:10
  tryCatch(foldrange(x, 11, function(a,b) a+b), error=function(e) TRUE)
})


#context("2D foldrange")
# TODO: Add tests here


#context("1D foldblock")
assert("Basic functionality",{
  x <- 1:12
  act <- foldblock(x, 3, function(a,b) mean(a) + b)
  exp <- sum(apply(matrix(x,nrow=3),2,mean))
  all.equal(act, exp)
})

assert("Equivalence with 2D fold",{
  x <- 1:12
  f <- function(a,b) mean(a) + b
  act <- foldblock(x, 3, f)
  exp <- fold(matrix(x,nrow=3), f, 0)
  all.equal(act, exp)
})

assert("Invalid window size",{
  x <- 1:10
  act <- foldblock(x, 11, function(a,b) a+b)
  exp <- x
  all.equal(act, exp)
})

assert("Window length does not divide length of x",{
  x <- 1:10
  act <- foldblock(x, 3, function(a,b) mean(a) + b)
  exp <- sum(apply(matrix(x[1:9],nrow=3),2,mean)) + 10
  all.equal(act, exp)
})


#context("2D foldblock")
assert("Summation operator over matrices",{
  ms <- matrix(sample(40,20, replace=TRUE), nrow=2)
  act <- foldblock(ms, 2, function(a,b) a + b)
  exp <- ms[,1:2] + ms[,3:4] + ms[,5:6] + ms[,7:8] + ms[,9:10]
  all.equal(act, exp)
})

