# :vim set filetype=R
#' Force values into bins
#'
#' This function quantizes data based on a metric function. The effect is that values
#' in x will be forced into the set of given bins.
#'
#' @name quantize
#' @param x The vector whose values should be binned
#' @param bins The available bins
#' @param metric The method to attract values to the bins
#'
#' @section Usage:
#' quantize(x, bins=c(-1,0,1), metric=function(a,b) abs(a-b))
#'
#' @section Details:
#' This function forces values into a set of bins using a distance metric. This process
#' is referred to as quantizing and is useful in signal processing or classification. The
#' metric used can be a function such as Euclidean distance or absolute distance, for 
#' example. 
#'
#' For each value in \code{x}, this \code{quantize} evaluates the metric function with
#' the value of \code{x} and all defined bins. The smallest metric value is selected 
#' and used to force the value of x to the nearest bin. The metric function should be
#' binary, otherwise a comparison between the value of x and the bin can not be made. 
#'
#' @return a vector containing quantized data.
#'
#' @examples
#' x <- seq(-2, 2, by=.1)  
#' quantize(x)
#'
#' # quantize x using a Euclidean distance metric.
#' quantize(x, metric=function(a, b) sqrt((a - b)^2))
#'
#' # Notice the difference in the return vector when compared to the above examples.
#' quantize(x, bins=c(-2, -1, 0, 1, 2), metric=function(a, b) sqrt((a - b)^2))
quantize(x, bins=c(-1,0,1), metric=function(a,b) abs(a-b)) %as% {
  bins <- sort(bins)
  ds <- sapply(bins, function(b) metric(x,b))
  if (is.null(dim(ds))) ds <- t(ds)
  apply(ds,1, function(d) item(bins, which.min(d)))
}


#' Confine values to the given bounds
#'
#' This function confines the values of a vector to the given bounds.
#'
#' @name confine
#' @param x A vector
#' @param min.level Lower bound on values to be confined
#' @param max.level Upper bound on values to be confined
#'
#' @section Usage:
#' confine(x, min.level=-1, max.level=1)
#'
#' @section Details:
#' This function confines a set of values within a lower and upper bound. The function
#' definition is only written for x being a vector and is not vectorized to handle 
#' two dimensional data structures. 
#'
#' @return A vector of points that lie within the bounds defined by min.level and 
#' max.level.
#'
#' @examples
#' # Confine the values in x to lie between [-1, 1].
#' x <- c(rep(4, 3), rep(3, 3), rep(0, 3), rep(-3, 3), rep(-4, 3))
#' confine(x)
#'
#' # Confine a random vector.
#' y <- rnorm(100, sd=4)
#' x <- 1:100
#' confine(y)
#'
#' # Note the effect of min.level and max.level.
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
#' @param x A sequence
#' @param pivot The index where x will be sliced
#' @param inclusive Defined as TRUE will include the value of x at index pivot as 
#' the first element in second half of the slice of x
#' @param expression A logical expression used for logical indexing of x
#' 
#' @section Usage:
#' slice(x, pivot, inclusive=FALSE)
#'
#' slice(x, expression)
#'
#' @section Details:
#' This function splits a sequence based on a pivot value or logical expression. 
#' The inclusive parameter will either include or exclude the value at the pivot 
#' in the second element of the returned list. 
#'
#' This function is defined for one- and two-dimensional data structures. In the two-
#' dimensional case, \code{slice} will return a list where each value in the list is a 
#' matrix. This function is useful for splitting data on a value and applying a function
#' to the smaller pieces.
#'
#' @return A list containing the left and right vectors with respect to pivot.
#'
#' @examples
#' # Slice x at the 25th index and switch inclusive flag - look at second half of the
#' # output.
#' x <- 1:10
#' slice(x, 5, TRUE)
#'
#' slice(x, 5, FALSE)
#'
#' # Some examples using expressions - Notice how the ordering of the returned 
#' # list of sequences changes.
#' slice(x, x < 5)
#'
#' slice(x, x > 5)
#'
#' # Slice a few two-dimensional objects.
#' A <- matrix(1:10, ncol=2)
#' slice(A, 3, TRUE)
#'
#' df <- data.frame(col1=1:10, col2=1:10)
#' slice(df, 5, TRUE)
#'
#' slice(df, 5, FALSE)
#'
#' slice(df, df$col1 < 5)
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
#' This function removes the head and tail of a data structure. Polymorphism is created 
#' around vectors, lists, matrices and data.frames.
#'
#' @name chomp
#' @param x Any indexable data structure
#' @param head The number of elements to be removed from the head of x
#' @param tail The number of elements to be removed from the tail of x
#' 
#' @section Usage:
#' chomp(x, head=1, tail=1)
#'
#' @section Details:
#' This function removes the head and tail of a data structure. The parameters
#' \code{head} and \code{tail} control how many elements are removed from the data
#' structure, and they must be positive. Additionally, the summation of \code{head} and
#' \code{tail} must not be greater than or equal to the length of the data structure.
#'  
#' @return A vector with the elements defined by head and tail removed.
#'
#' @examples
#' chomp(1:10)
#'
#' chomp(1:10, head=2, tail=2)
#'
#' chomp(matrix(1:10, ncol=2))
#'
#' chomp(data.frame(x=1:10, y=1:10, head=2, tail=2))
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
