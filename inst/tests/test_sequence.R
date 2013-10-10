context("pad")
test_that("pad pads corrrect number of NAs to head", {
  x <- 1:5
  y <- c(rep(NA, 5), 1:5)
  expect_equal(pad(x, 5), y)
})

test_that("pad pads corrrect number of NAs to tail", {
  x <- 1:5
  y <- c(1:5, rep(NA, 5))
  expect_equal(pad(x, 0, 5), y)
})

test_that("pad pads corrrect number of NAs to head and tail", {
  x <- 1:5
  y <- c(rep(NA, 5), 1:5, rep(NA, 5))
  expect_equal(pad(x, 5, 5), y)
})

test_that("pad pads corrrect number of 0s to head and tail", {
  x <- 1:5
  y <- c(rep(0, 5), 1:5, rep(0, 5))
  expect_equal(pad(x, 5, 5, default=0), y)
})

test_that("x can not be a 2-d array.", {
  x <- matrix(rnorm(10), ncol=2)
  expect_error(pad(x, 10), "No valid function for")
})


context("partition")
test_that("partition works for a small sequence", {
  x <- 1:5
  y <- matrix(c(1, 3, 5, 7, 5, 7, 9, 5), ncol=2)
  anynames(y) <- c("left", "right")
  expect_equal(partition(x, metric=sum, radius=1), y)
})

test_that("partition works for a small sequence and a radius of 4", {
  x <- 1:5
  y <- matrix(c(1, 3, 6, 10, 14, 12, 9, 5), ncol=2)
  anynames(y) <- c("left", "right")
  expect_equal(partition(x, metric=sum, radius=4), y)
})

test_that("x can not be a 2-d array", {
  x <- matrix(rnorm(10), ncol=2)
  expect_error(partition(x), "No valid function for")
})

test_that("Partition fails when metric does not map a vector to a scalar.", {
  x <- 1:50
  expect_error(partition(x, metric=cumsum), "No valid function for")
})


context("segment")
test_that("segment works for a small sequence", {
  x <- 1:5
  y <- data.frame(a=1:4, b=2:5)
  expect_equal(segment(x), y)
})

test_that("x can not be a 2-d array", {
  x <- matrix(rnorm(10), ncol=2)
  expect_error(segment(x), "No valid function for")
})

context("item")
test_that("item works for a small sequence", {
  v <- 1:10
  expect_equal(item(v, 5), 5) 
})

test_that("item works for a small sequence with bad index.", {
  v <- 1:10
  expect_equal(item(v, 0), NA) 
})

test_that("x can not be a 2-d array", {
  x <- matrix(rnorm(10), ncol=2)
  expect_error(item(x, 1), "No valid function for")
})


context("range.for")
test_that("range.for works for a sequence with repeating values", {
  sequence <- c(seq(1, 25), c(25, 25), seq(26, 50))
  y <- data.frame(min=25, max=27)
  expect_equal(range.for(25, sequence), y)
})

test_that("Series can not be a 2-d array", {
  x <- matrix(1:10, ncol=2)
  expect_error(range.for(2, x), "No valid function for")
})


context("samplerange")
test_that("Window can not be greater than the length of x", {
  x <- rnorm(10)
  expect_error(samplerange(x, 20, 50), "No valid function for") 
})
