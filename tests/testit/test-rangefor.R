# :vim set filetype=R
#context("range_for")
assert("range_for works for a sequence with repeating values", {
  sequence <- c(seq(1, 25), c(25, 25), seq(26, 50))
  y <- data.frame(min=25, max=27)
  all.equal(range_for(25, sequence), y)
})

assert("Series can not be a 2-d array", {
  x <- matrix(1:10, ncol=2)
  tryCatch(range_for(2, x), function(e) TRUE)
})


#context("samplerange")
assert("Window can not be greater than the length of x", {
  x <- rnorm(10)
  tryCatch(samplerange(x, 20, 50), function(e) TRUE) 
})
