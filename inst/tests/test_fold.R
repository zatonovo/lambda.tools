# :vim set filetype=R

context("fold")
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

context("foldrange")
test_that("foldrange with window size not a multiple of length(x)",{
  x <- 1:10
  y <- foldrange(x, 3, function(a,b) a+b)
  expect_equal(y, c(36, 44, 52))
})

test_that("foldrange with x a vector and with window size > length(x)",{
  x <- 1:10
  expect_error(foldrange(x, 11, function(a,b) a+b, "No valid function for"))
})

context("foldblock")
test_that("foldblock with x a vector and with block size < length(x)",{
  x <- 1:10
  y <- foldblock(x, 2, function(a,b) a+b)
  expect_equal(y, c(25, 30))
})

test_that("blocfold with x a vector and with block size > length(x)",{
  x <- 1:10
  expect_error(foldblock(x, 11, function(a,b) a+b))
})

test_that("foldblock with x a vector and with block size not a multiple of length(x)",{
  x <- 1:10
  y <- foldblock(x, 3, function(a,b) a+b)
  expect_equal(y, c(15, 18, 21))
})
