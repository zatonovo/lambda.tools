# :vim set filetype=R
#context("chomp")
assert("chomp works with a small sequence and default head/tail param", {
  x <- 1:10
  y <- 2:9
  all.equal(chomp(x), y)
})

assert("chomp works with a small sequence with different head/tail params", {
  x <- 1:10
  y <- 3:8
  all.equal(chomp(x, head=2, tail=2), y)
})

assert("chomp works with a matrix", {
  m <- matrix(1:10, ncol=2)
  y <- matrix(c(2, 3, 4, 7, 8, 9), ncol=2) 
  all.equal(chomp(m), y)
})

assert("chomp works on a data.frame", {
  df <- data.frame(x=1:10, y=1:10)
  y <- df[3:8,]
  all.equal(chomp(df, head=2, tail=2), y)
})

assert("Chomp with head and tail < 0 fails.", {
  x <- 1:50
  df <- data.frame(col1=x, col2=x)
  tryCatch(chomp(x, head=-1, tail=-1), error=function(e) TRUE)
  tryCatch(chomp(df, head=-1, tail=-1), error=function(e) TRUE)
})

assert("Head and tail parameters can not overlap.", {
  x <- 1:50
  df <- data.frame(col1=x, col2=x)
  tryCatch(chomp(x, head=26, tail=26), error=function(e) TRUE)
  tryCatch(chomp(df, head=1, tail=50), error=function(e) TRUE)
})
