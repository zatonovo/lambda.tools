# :vim set filetype=R

#' Pad a vector with NAs or other values.
#'
#' This function pads a vector with \code{NA}s or other values.
#'
#' @name pad
#' @param x Vector to be padded
#' @param head The length of padded values at the head of x
#' @param tail The length of padded values at the tail of x
#' @param default Value to be padded (default is NA)
#'
#' @section Details:
#' This function pads NAs or other values to the head and tail of a vector. 
#' This function has only been implemented for \code{x} being a vector. The 
#' number of elements that are padded to the head or tail of the vector are
#' controlled by the parameters \code{head} and \code{tail}, respectively.
#'
#' @return The input vector x will be returned with the appropriate value(s)
#' padded to the head or tail, or both.
#'
#' @examples
#' # Pad adds NAs to the head of x.
#' x <- 1:5
#' pad(x, 5) 
#' 
#' # Pad adds NAs to the tail of x. 
#' pad(x, 0, 5)
#'
#' # Pad add NAs to the head and tail of x. 
#' pad(x, 5, 5) 
#'
#' # Pad adds zeros to the head and tail of x.
#' pad(x, 5, 5, default=0)
pad(x, head, tail=0, default=NA) %when% {
    is.null(dim(x))
} %as% {
  c(rep(default,head),x, rep(default,tail))
}

#' Partition a sequence into coordinate pairs based on adjacent windows and
#' apply a metric function to each window
#'
#' This function partitions a sequence into coordinate pairs based on adjacent
#' windows and applies a function to each window.
#'
#' @name partition 
#' @param x A sequence
#' @param metric A function that maps a vector to a scalar
#' @param radius The maximum window size of the adjacent partitions
#' 
#' @section Details:
#' If the metric function does not map a vector to a scalar, \code{partition}
#' will throw an error. The metric function will be applied to each partition
#' and the value of the function will be returned for each panel in the 
#' resulting matrix (left or right panel).
#'
#' The radius parameter controls the size of the partitions. The partition size
#' of the windows remains constant until one of the windows approach the head
#' or tail of the sequence. In that case the respective window will have a
#' length less than the radius parameter.
#'
#' @return A matrix with columns left and right where each row corresponds to
#' the value of the metric function that was applied to adjacent windows.
#'  
#' @examples
#' x <- 1:5
#' partition(x, metric=sum, radius=1)
#'
#' # Apply a function to each window and change the radius.
#' partition(x, metric=sum, radius=4)
partition(x, metric=median, radius=10) %when% {
  is.null(dim(x))   
  length(metric(x)) == 1
} %as% {
  f <- function(x,i) {
    c(left=metric(x[max(1,i-radius):i]),
      right=metric(x[(i+1):min(length(x),i+1+radius)]))
  }
  t(sapply(1:(length(x)-1), function(i) f(x,i)))
}


#' Similar to partion where radius=2 and there is no metric
#'
#' @name segment
#' @param x a sequence
#' @param do.pad logical to control if NAs will be padded to segments
#' 
#' @section Details:
#' This function is only defined for a one-dimensional vector and will throw
#' an error if a two-dimensional data structure is used for input. The 
#' \code{segment} function will shift a sequence x by one element and return
#' a two-column \code{data.frame} containing the shifted sequences. 
#' 
#' Segmenting the sequences in this manner is useful for finding the 
#' boundaries of adjacent values.
#'  
#' @section Value:
#' Returns a two-column \code{data.frame} containing the shifted sequence. 
#'
#' @section See Also:
#' \code{range.for} 
#'
#' @examples
#' x <- 1:50
#' segment(x)
#'
#' segment(x, TRUE)
segment(x, do.pad=FALSE) %when% {
  is.null(dim(x))   
} %as% {
  x <- onlyif(do.pad, function(y) pad(y,1,1), x)
  data.frame(a=x[1:(length(x)-1)], b=x[2:length(x)])
}

#' Safely get an element from a vector
#'
#' This function returns an element from a vector or NA whenever a 
#' bad index is encountered.
#'
#' @name item
#' @param v A vector of length n
#' @param idx The index of the element to be returned by item
#' 
#' @section Usage:
#' item(v, idx)
#'
#' @section Details:
#' This function is designed to create a consistent return value for a bad index value.
#' If a bad index value is used \code{item} will return NA. 
#'
#' @return A scalar corresponding to the index used for the idx parameter
#'
#' @examples
#' # Get an element from v with a good and bad index - notice the return value 
#' # for the second funciton call.
#' v <- 1:10 
#' item(v, 5)
#'
#' item(v, 0) 
item(v, NA) %as% NA
item(v, idx) %when% { length(idx) == 0 } %as% NA
item(v, idx) %when% { idx == 0 } %as% NA
item(v, idx) %when% { is.null(dim(v)) } %as% v[idx]

#' Get the range of a value in a series
#'
#' This function returns the range of a repeated or non-repeated element in a series.
#'
#' @name range.for
#' @param value A value in series (can be unique or repeating in series)
#' @param series A series of values
#'
#' @section Details:
#' This function is only defined for a one-dimensional vector and will throw an
#' error if a two-dimensional input is used. The function \code{segment} is used 
#' to shift the series and scan the rows of the \code{data.frame} for matching values.
#' If two values are the same on a specific row then adjacent data can be detected.
#'
#' @return A \code{data.frame} where each row contains the start and end index for the 
#' range of the value of interest in the series.
#'
#' @section TODO:
#' Implement a \code{range.for} clause that can find the range of a subsequence 
#' in a sequence. For example,
#'
#' \code{sequence <- c(seq(1, 10), c(10, 10), seq(11, 20))}
#'
#' \code{range.for(c(10, 10, 10), sequence)}
#' 
#' The above call to \code{range.for} should return the range for the 
#' triplet \code{c(10, 10, 10)}.  
#'
#' @section See Also:
#' \code{segment}
#'
#' @examples
#' # Find the range of repeated values of 25 in a series.
#' series <- c(seq(1, 25), c(25, 25), seq(26, 50))
#' range.for(25, series) 
range.for(value, series) %when% {
  is.null(dim(series))
} %as% {
  x <- segment(series, TRUE)
  idx <- 1:nrow(x)
  idx.inf <- (is.na(x$a) | x$a != value) & (!is.na(x$b) & x$b == value)
  idx.sup <- (!is.na(x$a) & x$a == value) & (is.na(x$b) | x$b != value)
  data.frame(min=idx[idx.inf], max=idx[idx.sup]-1)
}

#' Get a sample as a subsequence of a larger set
#'
#' @name samplerange
#' @param x A one-dimensional or two-dimensional data structure
#' @param size Sample size
#' @param window The length of the output vectors 
#' @param \dots Optional arguments for the sample.int function
#'
#' @section Details:
#' If the window parameter is defined to be greater than the length of the input vector,
#' this function will throw an error. This function can be applied to both one- and
#' two-dimensional data structures.
#'
#' A fixed interger set of a sample size is sampled randomly based on the 
#' the length of the input vector \code{x} and the window parameter. This fixed integer
#' set is used to index the values in the vector x.
#'
#' @return A matrix containing the samples from x stored column-wise.
#'
#' @examples
#' # Sample a range in a vector.
#' x <- 1:10
#' samplerange(x, 5, 2)
#'
#' samplerange(x, 5, 3)
#'
#' # Sample a range in a matrix.
#' x <- matrix(rnorm(10), ncol=2)
#' samplerange(x, 4, 2)
samplerange(x, size, window, ...) %when% {
  is.null(dim(x))
  window < length(x)
} %as% {
  count <- length(x) - window + 1
  samples <- sample.int(count, size, ...)
  t(sapply(samples, function(s) x[s:(s+window-1)]))
}

# @return A list containing each window
samplerange(x, size, window, ...) %when% {
  window < length(x)
} %as% {
  count <- nrow(x) - window + 1
  samples <- sample.int(count, size, ...)
  lapply(samples, function(s) x[s:(s+window-1),])
}
