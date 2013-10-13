# :vim set filetype=R
context("segment")
test_that("No padding works", {
  x <- 1:5
  y <- matrix(c(1:4, 2:5), ncol=2, dimnames=list(NULL,c('a','b')))
  expect_equal(segment(x), y)
})

test_that("Using padding works", {
  x <- 1:5
  y <- matrix(c(NA,1:5, 1:5,NA), ncol=2, dimnames=list(NULL,c('a','b')))
  expect_equal(segment(x, TRUE), y)
})

test_that("x cannot be a 2-d array", {
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


