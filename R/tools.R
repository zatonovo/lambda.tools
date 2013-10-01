# :vim set filetype=R

#' Force values into bins
#'
#' @name quantize
#' @param x The vector whose values should be binned
#' @param bins The available bins
#' @param metric The method to attract values to the bins
#'
#' @examples
#' x <- seq(-2, 2, by=.1)  
#' quantize(x)
#' # quantize x using a Euclidian distance metric.
#' quantize(x, metric=function(a, b) sqrt((a - b)^2))
#' # quantize with custom bins. 
#' quantize(x, bins=c(-2, -1, 0, 1, 2), metric=function(a, b) sqrt((a - b)^2))
quantize(x, bins=c(-1,0,1), metric=function(a,b) abs(a-b)) %as% {
  bins <- sort(bins)
  ds <- sapply(bins, function(b) metric(x,b))
  if (is.null(dim(ds))) ds <- t(ds)
  apply(ds,1, function(d) item(bins, which.min(d)))
}


#' Confine values to the given bounds
#'
#' @name confine
#' @param x a vector
#' @param min.level lower bound on values to be confined
#' @param max.level upper bound on values to be confined
#'
#' @examples
#' x <- 1:100
#' y <- rnorm(100, sd=4)
#' confine(y)
#' # Illustration of confine(y) behavior.
#' plot(x, y)
#' points(x, confine(y), col='red') 
#' points(x, confine(y, min.level=-2, max.level=2), col='blue')
#' points(x, confine(y, min.level=-3, max.level=3), col='green') 
confine(x, min.level=-1, max.level=1) %when% {
  length(x) > 1
  min.level < max.level
} %as% {
  sapply(x, function(y) confine(y,min.level,max.level))
}

confine(x, min.level, max.level) %when% { x < min.level } %as% min.level
confine(x, min.level, max.level) %when% { x > max.level } %as% max.level
confine(x, min.level, max.level) %as% x


#' Split a sequence based on a pivot value or an expression
#'
#' @name slice
#' @param x a sequence
#' @param pivot the index where x will be sliced. 
#' @param inclusive defined as TRUE will include the value of x at index pivot as 
#' the first element in second half of the slice of x.
#' @param expression is a logical expression used for logical indexing of x. 
#' @examples
#' # Slice a one-dimensional sequence. 
#' x <- 1:50
#' slice(x, 25, TRUE)
#' slice(x, 25, FALSE)
#' slice(x, x < 25)
#' slice(x, x > 25)
#' slice(x, x > 10 & x < 25)
#' slice(x,  x > 10 & x < 25)
#' # Slice a matrix.
#' A <- matrix(1:10, ncol=2)
#' slice(A, 3, TRUE)
#' slice(A, 3, FALSE)
#' slice(A,  A[,1] > 5 & A[,1] < 7)
#' # Slice a data frame.
#' x <- 1:50
#' df <- data.frame(col1=x, col2=x)
#' slice(df, 25, TRUE)
#' slice(df, 25, FALSE)
#' slice(df, df$col2 > 30)
#' slice(df, df$col1 > 10 & df$col1 < 25)
slice(x, pivot, inclusive) %::% a : numeric : logical : list
slice(x, pivot, inclusive=FALSE) %when% {
  is.null(dim(x))
  pivot < anylength(x)
} %as% {
  left <- x[1:pivot]
  right <- x[(pivot+as.numeric(!inclusive)):length(x)]
  list(left, right)
}

slice(x, pivot, inclusive=FALSE) %when% {
  pivot < anylength(x)
} %as% {
  left <- x[1:pivot,]
  right <- x[(pivot+as.numeric(!inclusive)):nrow(x),]
  list(left, right)
}

slice(x, expression) %::% a : logical : list
slice(x, expression) %when% {
  is.null(dim(x))
  length(expression) == anylength(x)
} %as% {
  left <- x[expression]
  right <- x[!expression]
  list(left, right)
}

slice(x, expression) %when% {
  length(expression) == anylength(x) 
} %as% {
  left <- x[expression,]
  right <- x[!expression,]
  list(left, right)
}


#' Remove the head and tail of a data structure
#'
#' @name chomp
#' @param x a vector or a 2-d data structure.
#' @param head the number of elements to be removed from the head of x
#' @param tail the number of elements to be removed from the tail of x
#' 
#' @examples
#' chomp(rnorm(10))
#' chomp(rnorm(10), head=2, tail=2)
#' chomp(matrix(rnorm(20), ncol=2))
#' chomp(data.frame(x=rnorm(20), y=rnorm(20)), head=5, tail=5)
chomp(x, head=1, tail=1) %when% {
  is.null(dim(x))
  head > 0 
  tail > 0
  head + tail < anylength(x)
} %as% {
  x[(1+head):(length(x)-tail)]
}

chomp(x, head=1, tail=1) %when% {
  head > 0
  tail > 0
  head + tail < anylength(x)
} %as% {
  x[(1+head):(nrow(x)-tail), ]
}
