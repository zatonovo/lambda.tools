# :vim set filetype=R
range.for(value, series) %as% {
  x <- segment(series, TRUE)
  idx <- 1:nrow(x)
  idx.inf <- (is.na(x$a) | x$a != value) & (!is.na(x$b) & x$b == value)
  idx.sup <- (!is.na(x$a) & x$a == value) & (is.na(x$b) | x$b != value)
  data.frame(min=idx[idx.inf], max=idx[idx.sup]-1)
}

#' Similar to partion where radius=2 and there is no metric
segment(x, pad=FALSE) %as% {
  x <- onlyif(function(y) pad(y,1,1), x, pad)
  data.frame(a=x[1:(length(x)-1)], b=x[2:length(x)])
}

#' Split a sequence based on an expression
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


chomp(x, head=1, tail=1) %when% {
  is.null(dim(x))
} %as% {
  x[(1+head):(length(x)-tail)]
}

chomp(x, head=1, tail=1) %as% {
  x[(1+head):(length(x)-tail), ]
}


#' Partition a sequence into coordinate pairs based on adjacent windows
partition(x, metric=median, radius=10) %as% {
  f <- function(x,i) {
    c(left=metric(x[max(1,i-radius):i]), 
      right=metric(x[(i+1):min(length(x),i+1+radius)]))
  }
  t(sapply(1:(length(x)-1), function(i) f(x,i)))
}


use_default(NULL, default) %as% default
use_default(NA, default) %as% default
use_default(x, default) %as% x
