context("quantize")
test_that("Bin ordering does not affect ties.", {

  x <- -20:20 / 10
  bins <- c(-1, 0, 1)
   
  y1 <- quantize(x, bins=c(-1, 0, 1))
  y2 <- quantize(x, bins=c(1, 0, -1)) 
 
  expect_equal(y1, y2)
})


context("confine")
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
test_that("Pivot is larger than length of x.", {

  x <- 1:50
  df <- data.frame(x=x, y=x)

  expect_error(slice(1:50, 51), "No valid function for")
  expect_error(slice(df, 51), "No valid function for")
})

test_that("Expression length is not equal to length of x.", {

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
