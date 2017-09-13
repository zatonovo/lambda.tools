# :vim set filetype=R
#context("segment")
assert("Numeric vector", {
  act <- segment(1:5)
  exp <- data.frame(a=1:4, b=2:5)
  all.equal(act, exp)
})

assert("Numeric vector with padding", {
  act <- segment(1:5, do.pad=TRUE)
  exp <- data.frame(a=c(NA,1:5), b=c(1:5,NA))
  all.equal(act, exp)
})

assert("Date vector", {
  d <- Sys.Date() + 0:4
  act <- segment(d)
  exp <- data.frame(a=d[1:4], b=d[2:5])
  all.equal(act, exp)
})

assert("Disallow 2D data structures", {
  x <- matrix(rnorm(10), ncol=2)
  tryCatch(segment(x), error=function(e) TRUE)
})


#context("item")
assert("item works for a small sequence", {
  v <- 1:10
  all.equal(item(v, 5), 5) 
})

assert("item works for a small sequence with bad index.", {
  v <- 1:10
  all.equal(item(v, 0), NA) 
})

assert("x can not be a 2-d array", {
  x <- matrix(rnorm(10), ncol=2)
  tryCatch(item(x, 1), error=function(e) TRUE)
})


