context("anylength")
test_that("vector has correct length", {
  a <- c(1,2,3)
  expect_that(anylength(a), equals(3))
})


context("anytypes")
test_that("unnamed vector has correct type", {
  a <- c(1,2,3)
  expect_that(anytypes(a), equals("numeric"))
})

test_that("named vector has correct type", {
  a <- c(1,2,3)
  names(a) <- c('a','b','c')
  expect_that(anytypes(a), equals("numeric"))
})

test_that("named data.frame has correct type", {
  a <- data.frame(a=c(1,2,3), b=c("larry","mo","curly"), c=c(TRUE,FALSE,TRUE))
  ts <- anytypes(a)
  expect_that(names(ts), equals(c("a","b","c")))
  names(ts) <- NULL
  expect_that(ts, equals(c('numeric','factor','logical')))
})

test_that("unnamed data.frame has correct type", {
  a <- data.frame(c(1,2,3), c("larry","mo","curly"), c(TRUE,FALSE,TRUE))
  ts <- anytypes(a)
  # The data.frame will fill this in
  expect_that(! is.null(names(ts)), is_true())
  names(ts) <- NULL
  expect_that(ts, equals(c('numeric','factor','logical')))
})


context("is.bad")
test_that("list with bad values", {
  a <- list(a=1:3, b=NULL, c=NA, d='foo')
  e <- list(a=rep(FALSE,3), b=TRUE, c=TRUE, d=FALSE)
  expect_that(is.bad(a), equals(e) )
})

test_that("vector with NAs", {
  a <- c(1,NA,3)
  expect_that(is.bad(a), equals(c(FALSE,TRUE,FALSE)))
})

test_that("data.frame with NAs", {
  a <- data.frame(a=1:3, b=NA)
  e <- matrix(c(rep(FALSE,3), rep(TRUE,3)), ncol=2)
  colnames(e) <- c('a','b')
  expect_that(is.bad(a), equals(e))
})

test_that("data.frame that is empty", {
  a <- data.frame(a=NULL, b=NULL)
  expect_that(is.bad(a), equals(TRUE))
})

test_that("matrix with NAs", {
  a <- matrix(c(1:3, NA), ncol=2)
  e <- matrix(c(rep(FALSE,3), TRUE), ncol=2)
  expect_that(is.bad(a), equals(e))
})


context("is.empty")
test_that("non-empty vector", {
  a <- c(1,2,3)
  expect_that(is.empty(a), equals(FALSE))
})
test_that("empty vector", {
  a <- c()
  expect_that(is.empty(a), equals(TRUE))
})

test_that("non-empty list", {
  a <- list(a=1,2,3)
  expect_that(is.empty(a), equals(FALSE))
})
test_that("empty list", {
  a <- list()
  expect_that(is.empty(a), equals(TRUE))
})

test_that("non-empty data.frame", {
  a <- data.frame(a=1:3,b=2,c=3)
  expect_that(is.empty(a), equals(FALSE))
})
test_that("empty data.frame", {
  a <- data.frame(a=NULL, b=NULL)
  expect_that(is.empty(a), equals(TRUE))
})
