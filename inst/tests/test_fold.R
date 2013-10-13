# :vim set filetype=R

context("1D fold")
test_that("fold with x as a vector", {
  x <- 1:10
  expect_equal(fold(x, function(a,b) a+b), 55) 
})

test_that("fold with x as a vector and fn not a function", {
  x <- 1:10
  fn <- "I am not a function"
  expect_error(fold(x, fn), "No valid function for") 
})

test_that("fold with x as a vector of length 1.", {
  x <- c(1)
  expect_equal(fold(x, function(a,b) a+b), 1)
})


context("2D fold")
test_that("fold with x as a matrix.", {
  x <- matrix(1:10, ncol=2)
  y <- fold(x, function(a,b) a+b)
  expect_equal(y, c(7, 9, 11, 13, 15))
})

test_that("fold with x as a data.frame.", {
  x <- data.frame(x1=1:10, x2=1:10) 
  y <- fold(x, function(a,b) a+b)
  expect_equal(y, c(2 ,4, 6, 8, 10, 12, 14, 16, 18, 20))
})


context("1D foldrange")
test_that("foldrange with window size not a multiple of length(x)",{
  x <- 1:10
  y <- foldrange(x, 3, function(a,b) a+b)
  expect_equal(y, c(36, 44, 52))
})

test_that("foldrange with x a vector and with window size > length(x)",{
  x <- 1:10
  expect_error(foldrange(x, 11, function(a,b) a+b, "No valid function for"))
})


context("2D foldrange")
# TODO: Add tests here


context("1D foldblock")
test_that("Basic functionality",{
  x <- 1:12
  act <- foldblock(x, 3, function(a,b) mean(a) + b)
  exp <- sum(apply(matrix(x,nrow=3),2,mean))
  expect_equal(act, exp)
})

test_that("Equivalence with 2D fold",{
  x <- 1:12
  f <- function(a,b) mean(a) + b
  act <- foldblock(x, 3, f)
  exp <- fold(matrix(x,nrow=3), f)
  expect_equal(act, exp)
})

test_that("Invalid window size",{
  x <- 1:10
  act <- foldblock(x, 11, function(a,b) a+b)
  exp <- x
  expect_equal(act, exp)
})

test_that("Window length does not divide length of x",{
  x <- 1:10
  act <- foldblock(x, 3, function(a,b) mean(a) + b)
  exp <- sum(apply(matrix(x[1:9],nrow=3),2,mean)) + 10
  expect_equal(act, exp)
})


context("2D foldblock")
test_that("Summation operator over matrices",{
  ms <- matrix(sample(40,20, replace=TRUE), nrow=2)
  act <- foldblock(ms, 2, function(a,b) a + b)
  exp <- ms[,1:2] + ms[,3:4] + ms[,5:6] + ms[,7:8] + ms[,9:10]
  expect_equal(act, exp)
})

