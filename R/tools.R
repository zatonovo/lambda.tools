# :vim set filetype=R

#' Force values into bins
#'
#' @param x The vector whose values should be binned
#' @param bins The available bins
#' @param attractor The method to attract values to the bins
#'
#' @examples
#' x <- rnorm(10, sd=4)
#' quantize(x)
quantize(x, bins=c(-1,0,1), metric=function(a,b) abs(a-b)) %as% {
  ds <- sapply(bins, function(b) metric(x,b))
  if (is.null(dim(ds))) ds <- t(ds)
  apply(ds,1, function(d) item(bins, which.min(d)))
}

.confine(x, min.level, max.level) %when% { x < min.level } %as% min.evel
.confine(x, min.level, max.level) %when% { x > max.level } %as% max.level
.confine(x, min.level, max.level) %as% x

#' Confine values to the given bounds
#' @examples
#' x <- rnorm(10, sd=4)
#' confine(x)
confine(x, min.level=-1, max.level=1) %as%
  sapply(x, function(y) .confine(y,min.level,max.level))



#' Split a sequence based on an expression
#'
#' @param x
#' @param pivot
#' @param inclusive
#' 
#' @examples
slice(x, pivot, inclusive) %::% a : numeric : logical : list
slice(x, pivot, inclusive=FALSE) %when% {
  is.null(dim(x))
} %as% {
  left <- x[1:pivot]
  right <- x[(pivot+as.numeric(!inclusive)):length(x)]
  list(left, right)
}

slice(x, pivot, inclusive=FALSE) %as%
{
  left <- x[1:pivot,]
  right <- x[(pivot+as.numeric(!inclusive)):nrow(x),]
  list(left, right)
}

slice(x, expression) %::% a : logical : list
slice(x, expression) %when% {
  is.null(dim(x))
} %as% {
  left <- x[expression]
  right <- x[!expression]
  list(left, right)
}

slice(x, expression) %as%
{
  left <- x[expression,]
  right <- x[!expression,]
  list(left, right)
}


#' Remove the head an tail of a data structure
#'
#' @param x
#' @param head
#' @param tail
#' 
#' @examples
#' chomp(rnorm(10))
#'
#' chomp(matrix(rnorm(20), ncol=2))
chomp(x, head=1, tail=1) %when% {
  is.null(dim(x))
} %as% {
  x[(1+head):(length(x)-tail)]
}

chomp(x, head=1, tail=1) %as% {
  x[(1+head):(length(x)-tail), ]
}


