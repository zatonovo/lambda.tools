#context("anylength")
assert("vector has correct length", {
  a <- c(1,2,3)
  all.equal(anylength(a), 3)
})

assert("matrix has correct length", {
  m <- matrix(1:10, ncol=2)
  all.equal(anylength(m), 5)
})

assert("data.frame has correct length", {
  df <- data.frame(x1=1:10, x2=1:10)
  all.equal(anylength(df), 10)
})


#context("anynames")
assert("named matrix has correct names", {
  m <- matrix(c(1,2,3,4,5,6), ncol=2)
  anynames(m) <- c('d','e')
  all.equal(anynames(m), c('d', 'e'))
})

assert("named vector has correct names", {
  v <- c(a=1,b=2,c=3,d=4,e=5)
  all.equal(anynames(v), c('a', 'b', 'c', 'd', 'e'))
})

assert("a named list has correct names", {
  l <- list(a=1,b=2,c=3,d=4,e=5)
  all.equal(anynames(l), c('a', 'b', 'c', 'd', 'e'))
})

assert("a named data.frame has correct names", {
  df <- data.frame(a=1:10, b=1:10,c=1:10,d=1:10,e=1:10)
  all.equal(anynames(df), c('a', 'b', 'c', 'd', 'e'))
})


#context("anytypes")
assert("A named data.frame has the correct types", {
  a <- data.frame(a=c(1,2,3), b=c("larry","mo","curly"), c=c(TRUE,FALSE,TRUE))
  ts <- anytypes(a)
  all.equal(names(ts), c("a","b","c"))
  names(ts) <- NULL
  all.equal(ts, c('numeric','factor','logical'))
})

assert("An unnamed data.frame has the correct types", {
  a <- data.frame(c(1,2,3), c("larry","mo","curly"), c(TRUE,FALSE,TRUE))
  ts <- anytypes(a)
  # The data.frame will fill this in
  ! is.null(names(ts))
  names(ts) <- NULL
  all.equal(ts, c('numeric','factor','logical'))
})


#context("is.bad")
assert("A list is handled correctly", {
  a <- list(a=1:3, b=NULL, c=NA, d='foo')
  e <- list(a=rep(FALSE,3), b=TRUE, c=TRUE, d=FALSE)
  all.equal(is.bad(a), e)
})

assert("A vector with NAs is handled correctly", {
  a <- c(1,NA,3)
  all.equal(is.bad(a), c(FALSE,TRUE,FALSE))
})

assert("A data.frame with NAs is handled correctly", {
  a <- data.frame(a=1:3, b=NA)
  e <- matrix(c(rep(FALSE,3), rep(TRUE,3)), ncol=2)
  colnames(e) <- c('a','b')
  all.equal(is.bad(a), e)
})

assert("A data.frame that is empty is handled correctly", {
  a <- data.frame(a=NULL, b=NULL)
  is.bad(a)
})

assert("A matrix with NAs is handled correctly", {
  a <- matrix(c(1:3, NA), ncol=2)
  e <- matrix(c(rep(FALSE,3), TRUE), ncol=2)
  all.equal(is.bad(a), e)
})


#context("is.empty")
assert("A non-empty vector resolves to FALSE", {
  a <- c(1,2,3)
  !is.empty(a)
})

assert("An empty vector resolves to TRUE", {
  a <- c()
  is.empty(a)
})

assert("A non-empty list resolves to FALSE", {
  a <- list(a=1,2,3)
  !is.empty(a)
})

assert("An empty list resolves to TRUE", {
  a <- list()
  is.empty(a)
})

assert("A non-empty data.frame resolves to FALSE", {
  a <- data.frame(a=1:3,b=2,c=3)
  !is.empty(a)
})

assert("An empty data.frame resolves to TRUE", {
  a <- data.frame(a=NULL, b=NULL)
  is.empty(a)
})
