# :vim set filetype=R
#' Force values into a set of bins
#'
#' This function quantizes data into a set of bins based on a 
#' metric function. Each value in the input is evaluated with each
#' quantization level (the bin), and the level with the smallest
#' distance is assigned to the input value.
#'
#' @section Usage:
#' quantize(x, bins=c(-1,0,1), metric=function(a,b) abs(a-b))
#'
#' @section Details:
#' When converting analog signals to digital signals, 
#' quantization is a natural phenomenon. This concept can be extended
#' to contexts outside of DSP. More generally it can be thought of
#' as a way to classify a sequence of numbers according to some
#' arbitrary distance function.
#' 
#' The default distance function is the Euclidean distance in 1 dimension.
#' For the default set of bins, values from (-infty, -.5] will map to -1.
#' The values from (-.5, .5] map to 0, and the segment (.5, infty) map to 1.
#' Regardless of the ordering of the bins, this behavior is
#' guaranteed. Hence for a collection of boundary points k and bins b,
#' where |b| = |k| + 1, the mapping will always have the form
#' (-infty, k_1] => b_1, (k_1, k_2] => b_2, ... (k_n, infty) => b_n.
#'
#' @name quantize
#' @param x A sequence
#' @param bins The bins to quantize into
#' @param metric The method to attract values to the bins
#' @return A vector containing quantized data
#'
#' @author Brian Lee Yung Rowe
#' @seealso \code{\link{confine}}
#'
#' @examples
#' x <- seq(-2, 2, by=.1)  
#' quantize(x)
#'
#' quantize(x, bins=-1.5:1.5)
quantize(x, bins=c(-1,0,1), metric=function(a,b) abs(a-b)) %as% {
  bins <- sort(bins)
  ds <- sapply(bins, function(b) metric(x,b))
  ds <- onlyif(is.null(dim(ds)), t, ds)
  apply(ds,1, function(d) item(bins, which.min(d)))
}


#' Confine values to the given bounds
#'
#' Given a sequence this function confines the sequence values to within
#' the specified bounds. This behavior is equivalent to clipping in
#' digital signal processing.
#'
#' @section Usage:
#' confine(x, min.level=-1, max.level=1)
#'
#' @section Details:
#' The confine function can be thought of a transform that limits the
#' range of a sequence. Any values outside the range [min.level, max.level]
#' are adjusted to be exactly min.level or max.level.
#' 
#' Care should be taken when using this function as it is not always
#' a good idea to change the value of outliers. Sometimes it is better
#' to remove these values from a data set instead.
#'
#' @name confine
#' @param x A numeric vector
#' @param min.level The lower bound
#' @param max.level The upper bound
#' @return A sequence with values outside of [min.level, max.level]
#' clipped to those values
#'
#' @author Brian Lee Yung Rowe
#' @seealso \code{\link{quantize}}
#'
#' @examples
#' confine(seq(-2,2, by=.1))
#'
confine(x, min.level=-1, max.level=1) %when% {
  min.level < max.level
} %as% {
  y <- ifelse(x < min.level, min.level, x)
  ifelse(y > max.level, max.level, x)
}


#' Slice a sequence into two adjacent sub-sequences
#'
#' A sequence can be sliced using an explicit pivot point or by using
#' a logical expression.
#'
#' @section Usage:
#' slice(x, pivot, inclusive=FALSE)
#'
#' slice(x, expression)
#'
#' @section Details:
#' This function splits a sequence into two adjacent sub-sequences
#' at a pivot point or based on a logical expression. If a pivot
#' point is chosen, then the inclusive parameter determines whether
#' the value associated with the pivot should be included in both
#' sub-sequences. If FALSE, then the indices of the sub-sequences 
#' will have the form [1, pivot], [pivot + 1, n], where n = |x|. If
#' inclusive is TRUE, then the sub-sequences have indices of
#' [1, pivot], [pivot, n]. Obviously the pivot must be an element
#' of the set of indices of x.
#'
#' An alternative construction is to use an expression to define
#' a slice point. The first sub-sequence corresponds to the
#' values where the expression evaluated to TRUE, while the 
#' second sequence corresponds to values when the expression 
#' evaluated to FALSE.
#'
#' In two dimensions only the first variant of this function is
#' defined, as it cannot be guaranteed that a regular matrix will
#' be generated using an arbitrary expression.
#'
#' @name slice
#' @param x An indexable data structure, typically a vector
#' @param pivot The index of the pivot point in x
#' @param inclusive Whether to include the pivot point in the second 
#'  sub-sequence
#' @param expression A logical expression
#' @return A list containing two sub-sequences or sub-matrices
#'
#' @author Brian Lee Yung Rowe
#'
#' @examples
#' # The number 4 is included in each sub-sequence
#' slice(1:10, 4, TRUE)
#'
#' # With expressions, the sub-sequences are not necessarily continguous
#' slice(x, x %% 2 == 0)
#'
#' # Same as above but in two dimensions
#' x <- matrix(1:40, ncol=4)
#' slice(x, 4)
#'
slice(x, pivot, inclusive) %::% a : numeric : logical : list
slice(x, pivot, inclusive=FALSE) %when% {
  is.null(dim(x))
  pivot > 0
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
  right <- x[(pivot+as.numeric(!inclusive)):anylength(x),]
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


