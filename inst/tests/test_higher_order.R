context("map")
test_that("map for a vector", {
  x <- 1:10
  expect_equal(map(x, function(a) sum(a)), x)
})

test_that("map for a vector using quantize", {
  y <- c(-1, -1, -1, -1, -1, -1,
         -1, -1, -1, -1, 0, 1, 1,
         1, 1, 1, 1, 1, 1, 1, 1)
  expect_equal(map(-10:10, quantize), y)
})


test_that("map throws error when fn is not a function", {
  x <- c(seq(1, 10, by=0.1), rep(NA, 5)) 
  f <- 'I am not a function.'
  expect_error(map(x, f))
})

test_that("map for a vector with NAs works", {
  x <- c(seq(1, 10, by=0.1), rep(NA, 5)) 
  f <- function(a) { a } 
  y <- map(x, f, y=c())
  expect_equal(x, y)
})

test_that("map with x as a matrix", {
  x <- matrix(1:10, ncol=2)
  z <- map(x, function(a) sum(a))
  expect_equal(z, c(15, 40))
})

test_that("map with x as a data.frame works for fn: Rn -> R", {
  x <- data.frame(col1=1:10, col2=1:10)
  z <- map(x, function(a) sum(a), y=list())
  expect_equal(z, list(55, 55))
})

context("maprange")
test_that("maprange with window size a multiple of length(x)",{
  x <- 1:10
  y <- maprange(x, 2, function(a) sum(a))
  expect_equal(y, c(3, 5, 7, 9, 11, 13, 15, 17, 19))
})

test_that("maprange with window size not a multiple of length(x)",{
  x <- 1:10
  y <- maprange(x, 3, function(a) sum(a))
  expect_equal(y, c(6, 9, 12, 15, 18, 21, 24, 27))
})

test_that("maprange with window size not a multiple of length(x) and do.pad",{
  x <- 1:10
  y <- maprange(x, 3, function(a) sum(a), TRUE)
  expect_equal(y, c(NA, NA, 6, 9, 12, 15, 18, 21, 24, 27))
})


test_that("maprange with x a vector and with window size > length(x)",{
  x <- 1:10
  expect_error(maprange(x, 11, function(a) sum(a), "No valid function for"))
})

context("mapblock")
test_that("mapblock with x a vector and with block size < length(x)",{
  x <- 1:10
  y <- mapblock(x, 2, mean)
  expect_equal(y, c(1.5, 3.5, 5.5, 7.5, 9.5))
})

test_that("mapblock with x a vector and with block size < length(x) and NAs",{
  x <- 1:10
  y <- mapblock(x, 2, mean, TRUE)
  expect_equal(y, c(NA, 1.5, 3.5, 5.5, 7.5, 9.5))
})

test_that("mapblock with x a vector and with block size = length(x) / 2",{
  x <- 1:10
  y <- mapblock(x, 5, mean)
  expect_equal(y, c(3, 8))
})

test_that("mapblock with x a vector and with block size = length(x) / 2 and NAs",{
  x <- 1:10
  y <- mapblock(x, 5, mean, TRUE)
  expect_equal(y, c(NA, NA, NA, NA, 3, 8))
})

test_that("mapblock with x a vector and with block size > length(x)",{
  x <- 1:10
  expect_error(mapblock(x, 11, function(a) sum(a)))
})

test_that("mapblock with x a vector and with block size not a multiple of length(x)",{
  x <- 1:10
  y <- mapblock(x, 3, function(a) sum(a))
  expect_equal(y, c(6, 15, 24, NA))
})

test_that("mapblock with x a matrix block size of one",{
  m <- matrix(1:12, ncol=2)
  y <- mapblock(m, 1, sum)
  expect_equal(y, c(21, 57))
})

test_that("mapblock with x a matrix block size equal to ncol(m)",{
  m <- matrix(1:12, ncol=2)
  y <- mapblock(m, 2, sum)
  expect_equal(y, c(78)) 
})

test_that("mapblock with x a data.frame block size of one",{
  df <- data.frame(col1=1:6, col2=7:12)
  y <- mapblock(df, 1, sum)
  expect_equal(y, c(21, 57))
})


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
