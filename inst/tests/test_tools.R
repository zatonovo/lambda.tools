context("quantize")
test_that("quanize works for a small sequence and default metric", {
  x <- seq(-2, 2, by=.1)
  y <- c(rep(-1, 16), rep(0, 10), rep(1, 15))
  expect_equal(quantize(x), y)
})

test_that("quanize works for a small sequence and Euclidean distance metric", {
  x <- seq(-2, 2, by=.1)
  y <- c(rep(-1, 16), rep(0, 10), rep(1, 15))
  expect_equal(quantize(x), y)
})

test_that("Bin ordering does not affect ties.", {
  x <- -20:20 / 10
  bins <- c(-1, 0, 1)
  y1 <- quantize(x, bins=c(-1, 0, 1))
  y2 <- quantize(x, bins=c(1, 0, -1)) 
  expect_equal(y1, y2)
})


context("confine")
test_that("confine works for a small sequence", {
  x <- c(rep(4, 3), rep(3, 3), rep(0, 3), rep(-3, 3), rep(-4, 3))
  y <- c(rep(1, 3), rep(1, 3), rep(0, 3), rep(-1, 3), rep(-1, 3)) 
  expect_equal(confine(x), y)
})

test_that("Min.level must be less than max.level.", {

  x <- 1:100
  y <- rnorm(100, sd=4)

  expect_error(confine(y, min.len=1, max.len=-1), "No valid function for")
  expect_error(confine(y, min.len=1, max.len=1), "No valid function for")
})

test_that("Confine fails for character input.", {

  x <- 'abcdefghijk'

  expect_error(confine(x), "No valid function for")
  expect_error(confine(x, min.len='b', max.len='h'), "No valid function for")
})


context("slice")
test_that("slice works for a sequence with inclusive flag set to FALSE", {
  x <- 1:10
  y <- list(c(1:5), c(6:10)) 
  expect_equal(slice(x, 5), y)
})

test_that("slice works for a sequence with inclusive flag set to TRUE", {
  x <- 1:10
  y <- list(c(1:5), c(5:10)) 
  expect_equal(slice(x, 5, TRUE), y)
})

test_that("slice works for a sequence and an expression", {
  x <- 1:10
  y <- list(c(1:4), c(5:10))
  expect_equal(slice(x, x < 5), y)
})

test_that("slice works for a sequence and another expression", {
  x <- 1:10
  y <- list(c(6:10), c(1:5))
  expect_equal(slice(x, x > 5), y)
})

test_that("slice works for a matrix", {
  A <- matrix(1:10, ncol=2)
  y <- list(matrix(c(1,2,3,6,7,8),ncol=2), matrix(c(3,4,5,8,9,10), ncol=2))
  expect_equal(slice(A, 3, TRUE), y)
})

test_that("slice works for a data.frame inclusive", {
  df <- data.frame(col1=1:10, col2=1:10)
  y <- list(df[1:5,], df[5:10,])
  expect_equal(slice(df, 5, TRUE), y)
})

test_that("slice works for a data.frame non-inclusive", {
  df <- data.frame(col1=1:10, col2=1:10)
  y <- list(df[1:5,], df[6:10,])
  expect_equal(slice(df, 5, FALSE), y)
})

test_that("slice works for a data.frame with expression", {
  df <- data.frame(col1=1:10, col2=1:10)
  y <- list(df[1:4,], df[5:10,])
  expect_equal(slice(df, df$col1 < 5), y)
})

test_that("pivot is larger than length of x", {
  x <- 1:50
  df <- data.frame(x=x, y=x)
  expect_error(slice(1:50, 51), "No valid function for")
  expect_error(slice(df, 51), "No valid function for")
})

test_that("expression length is not equal to length of x", {
  x <- 1:50
  x2 <- 1:100
  df <- data.frame(col1=x, col2=x) 
  df2 <- data.frame(col1=x2, col2=x2)
  expect_error(slice(x, x2 < 25), "No valid function for")
  expect_error(slice(x, x2 < 25 & x2 > 50), "No valid function for")
  expect_error(slice(df, df2$col1 < 25 & df2$col1 > 50), "No valid function for")
  expect_error(slice(df, df2[,1] < 25 & df2[,1] > 50), "No valid function for")
})


context("chomp")
test_that("chomp works with a small sequence and default head/tail param", {
  x <- 1:10
  y <- 2:9
  expect_equal(chomp(x), y)
})

test_that("chomp works with a small sequence with different head/tail params", {
  x <- 1:10
  y <- 3:8
  expect_equal(chomp(x, head=2, tail=2), y)
})

test_that("chomp works with a matrix", {
  m <- matrix(1:10, ncol=2)
  y <- matrix(c(2, 3, 4, 7, 8, 9), ncol=2) 
  expect_equal(chomp(m), y)
})

test_that("chomp works on a data.frame", {
  df <- data.frame(x=1:10, y=1:10)
  y <- df[3:8,]
  expect_equal(chomp(df, head=2, tail=2), y)
})

test_that("Chomp with head and tail < 0 fails.", {
  x <- 1:50
  df <- data.frame(col1=x, col2=x)
  expect_error(chomp(x, head=-1, tail=-1), "No valid function for")
  expect_error(chomp(df, head=-1, tail=-1), "No valid function for")
})

test_that("Head and tail parameters can not overlap.", {
  x <- 1:50
  df <- data.frame(col1=x, col2=x)
  expect_error(chomp(x, head=26, tail=26), "No valid function for")
  expect_error(chomp(df, head=1, tail=50), "No valid function for")
})
