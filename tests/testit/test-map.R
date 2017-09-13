# :vim set filetype=R
#context("1D map")
assert("Basic functionality", {
  x <- 1:10
  act <- map(x, function(a) a^2)
  exp <- x^2
  all.equal(act, exp)
})

assert("Custom accumulator", {
  x <- 1:10
  act <- map(x, function(a) a^2, acc=list())
  exp <- as.list(x^2)
  all.equal(act, exp)
})

assert("Invalid functions are not allowed", {
  x <- 1:10
  f <- 'I am not a function.'
  tryCatch(map(x, f), error=function(e) TRUE)
})

assert("NAs okay in input sequence", {
  x <- c(-5:5, NA, NA, 7:10)
  act <- map(x, function(a) abs(a))
  exp <- abs(x)
  all.equal(act, exp)
})


#context("2D map")
assert("Basic functionality", {
  x <- matrix(1:10, ncol=2)
  act <- map(x, function(a) sum(a))
  exp <- apply(x, 2, sum)
  all.equal(act, exp)
})

assert("Use a data.frame as input", {
  x <- data.frame(a=1:10, b=11:20)
  act <- map(x, function(a) sum(a))
  exp <- c(55, 155)
  all.equal(act, exp)
})

assert("Use a data.frame as input and accumulate in a list", {
  x <- data.frame(a=1:10, b=11:20)
  act <- map(x, function(a) sum(a), acc=list())
  exp <- list(55, 155)
  all.equal(act, exp)
})


#context("1D maprange")
assert("maprange with window size a multiple of length(x)",{
  x <- 1:10
  y <- maprange(x, 2, function(a) sum(a))
  all.equal(y, c(3, 5, 7, 9, 11, 13, 15, 17, 19))
})

assert("maprange with window size not a multiple of length(x)",{
  x <- 1:10
  y <- maprange(x, 3, function(a) sum(a))
  all.equal(y, c(6, 9, 12, 15, 18, 21, 24, 27))
})

assert("maprange with window size not a multiple of length(x) and do.pad",{
  x <- 1:10
  y <- maprange(x, 3, function(a) sum(a), TRUE)
  all.equal(y, c(NA, NA, 6, 9, 12, 15, 18, 21, 24, 27))
})

assert("maprange with x a vector and with window size > length(x)",{
  x <- 1:10
  tryCatch(maprange(x, 11, function(a) sum(a)), error=function(e) TRUE)
})

assert("maprange with window size a multiple of length(x) and window gap < window size", {
  x <- 1:10
  y <- maprange(x, 3, function(a) sum(a), by = 2)
  all.equal(y, c(6, 12, 18, 24))
})

assert("maprange with window size a multiple of length(x) and window gap > window size", {
  x <- 1:10
  y <- quote(maprange(x, 3, function(a) sum(a), by = 4))
  tryCatch(eval(y), error=function(e) TRUE)
})

#context("2D maprange")
# TODO: Add tests here


#context("1D mapblock")
assert("Basic functionality",{
  x <- 1:10
  act <- mapblock(x, 2, mean)
  exp <- apply(matrix(x,nrow=2), 2, mean)
  all.equal(act, exp)
})

assert("Window does not divide length of x",{
  x <- 1:10
  act <- mapblock(x, 3, mean)
  exp <- c(apply(matrix(x[1:9],nrow=3), 2, mean), 10)
  all.equal(act, exp)
})

assert("Window longer than length of x",{
  x <- 1:10
  act <- mapblock(x, 11, mean)
  exp <- mean(x)
  all.equal(act, exp)
})


#context("2D mapblock")
assert("mapblock with x a matrix block size of one",{
  m <- matrix(1:12, ncol=2)
  y <- mapblock(m, 1, sum)
  all.equal(y, c(21, 57))
})

assert("mapblock with x a matrix block size equal to ncol(m)",{
  m <- matrix(1:12, ncol=2)
  y <- mapblock(m, 2, sum)
  all.equal(y, c(78))
})

assert("mapblock with x a data.frame block size of one",{
  df <- data.frame(col1=1:6, col2=7:12)
  y <- mapblock(df, 1, sum)
  all.equal(y, c(21, 57))
})


