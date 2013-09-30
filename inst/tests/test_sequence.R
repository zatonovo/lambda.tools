context("pad")
test_that("x can not be a 2-d array.", {

  x <- matrix(rnorm(10), ncol=2)

  expect_error(pad(x, 10), "No valid function for")
})


context("partition")
test_that("x can not be a 2-d array.", {

  x <- matrix(rnorm(10), ncol=2)

  expect_error(partition(x), "No valid function for")
})

test_that("Partition fails when metric does not map a vector to a scalar.", {
  
  x <- 1:50

  expect_error(partition(x, metric=cumsum), "No valid function for")
})


context("segment")
test_that("x can not be a 2-d array.", {

  x <- matrix(rnorm(10), ncol=2)

  expect_error(segment(x), "No valid function for")
})

context("item")
test_that("x can not be a 2-d array.", {

  x <- matrix(rnorm(10), ncol=2)

  expect_error(item(x, 1), "No valid function for")
})


context("range.for")
test_that("Series can not be a 2-d array.", {

  x <- matrix(1:10, ncol=2)

  expect_error(range.for(2, x), "No valid function for")

})


context("samplerange")
test_that("Window can not be greater than the length of x.", {

  x <- rnorm(10)

  expect_error(samplerange(x, 20, 50), "No valid function for") 
})
