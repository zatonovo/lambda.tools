# :vim set filetype=R

#' Pad a vector with some default value
#'
#' This function pads a vector with default values as a way to coerce the
#' value to some predetermined length.
#'
#' @section Usage:
#' pad(x, head, tail=0, default=NA)
#'
#' @section Details:
#' It is common for sequence operations to return a sequence that is
#' shorter than the original sequence. This phenomenon can be
#' annoying when binding the output with the input in a regular
#' data structure like a matrix or data.frame. This function prepends
#' or appends a specified value to a data structure to ensure that the
#' length of the data structure is compatible with another data structure.
#'
#' @name pad
#' @param x A vector to pad
#' @param head The amount to prepend
#' @param tail The amount to append
#' @param default The value to use for the pad
#' @return A padded sequence
#'
#' @author Brian Lee Yung Rowe
#'
#' @examples
#' # A moving average results in n - window + 1 results, so pad at the
#' # head to get a vector of length 50
#' x <- abs(rnorm(50))
#' m <- maprange(x, 10, mean)
#' pad(m, 9) 
#' 
#' # Pad at the end instead of the beginning. Note that the head must
#' # explicitly be set to 0
#' pad(m, 0, 9)
#'
#' # Pad on both sides
#' pad(m, 4, 5) 
#'
#' # Use a different default value
#' pad(m, 9, default=0)
pad(x, head, tail=0, default=NA) %when% {
  is.null(dim(x))
} %as% {
  c(rep(default,head),x, rep(default,tail))
}


#' Remove the head and tail of a data structure
#'
#' Remove the specified number of elements from either the head or
#' tail of a data structure. 
#'
#' @section Usage:
#' chomp(x, head=1, tail=1)
#'
#' @section Details:
#' This function is inspired by the PERL function of the same name. While
#' the PERL version is designed for strings, this version is designed for
#' any indexable data structure, typically containing numbers.
#'
#' @name chomp
#' @param x Any indexable data structure
#' @param head The number of elements to be removed from the head of x
#' @param tail The number of elements to be removed from the tail of x
#' @return A data structure with the head and tail chomped off
#'
#' @author Brian Lee Yung Rowe
#' @seealso \code{\link{pad}}
#'
#' @examples
#' chomp(1:10)
#' chomp(letters)
#'
#' chomp(data.frame(x=1:10, y=1:10), head=2, tail=2)
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


#' Partition a sequence into adjacent windows and apply a metric function 
#' to each window
#'
#' This function transforms a sequence into a rolling set of adjacent
#' windows separated by a pivot point. Each window is passed to a 
#' metric function that yields a scalar value. The result is effectively
#' a coordinate pair that represents the two adjacent windows.
#'
#' @section Usage:
#' partition(x, metric, radius) %::% . : Function : numeric : matrix
#' partition(x, metric=median, radius=10)
#' 
#' @section Details:
#' Many analysis approaches explore ways to reduce the dimensionality
#' of a data set to make it easier to model. The opposite situation is 
#' when there is not enough information in the data structure as is.
#' This circumstance requires a technique that can add dimensionality
#' to a data structure, which is what this function does.
#'
#' The idea is that a sequence can yield additional information by
#' comparing the neighborhoods around a given point. For this function,
#' a point is an index of the sequence. In the 1D case, given 
#' an index k and a radius r, the left neighbohood is defined by
#' [k-r+1, k] and the right neighborhood is defined by
#' [k+1, k+r]. The values associated with each neighborhood
#' are then applied to a metric function m: A^r -> R.
#' This output becomes the coordinate pair (left, right).
#'
#' At the edges of the sequence the above formalism is not completely
#' accurate. This is because at the edge, the neighborhood will be 
#' smaller than the radius, with a minimum size of 1.
#' Hence the first iteration on a sequence will yield a left neighborhood
#' of 1, while the right neighborhood will be [2, 1+r]. Whether this is
#' acceptable is case-specific.
#'
#' In the future, a wrap parameter might be included that would emulate
#' a loop instead of a sequence. This would be useful if a sequence
#' represented a stationary time series.
#'
#' @name partition 
#' @param x A sequence
#' @param metric A function that maps a vector to a real-valued scalar
#' @param radius The extent of the neighborhood about the index point
#' 
#' @return A length(x)-1 by 2 matrix where each row represents
#' the value of the metric applied to left and right neighborhoods
#' about an index point.
#'  
#' @author Brian Lee Yung Rowe
#'
#' @examples
#' partition(1:10, mean, radius=2)
#'
partition(x, metric, radius) %::% . : Function : numeric : matrix
partition(x, metric=median, radius=10) %when% {
  is.null(dim(x))
} %as% {
  f <- function(x,i) {
    c(left=metric(x[max(1,i-radius+1):i]),
      right=metric(x[(i+1):min(length(x),i+radius)]))
  }
  t(sapply(1:(length(x)-1), function(i) f(x,i)))
}


#' Segment a sequence into shifted versions of itself
#'
#' Create a shifted version of a sequence to make it easier to do
#' certain types of analysis.
#'
#' @section Usage:
#' segment(x, do.pad=FALSE)
#'
#' @section Details:
#' Segmenting sequences into offset versions of itself is useful for
#' detecting patterns in sequences. This approach is compatible with
#' a functional programming style as each row can then be passed to
#' a map-vectorized function for further processing.
#'  
#' The advantage over an iterative approach is that the map-vectorized
#' function can focus on a row-specific model independent of data
#' management mechanics like maintaining proper indices while iterating
#' over the sequence, as this is handled by segment.
#'
#' @name segment
#' @param x A vector
#' @param do.pad Whether the vector should be padded to contain 
#' the edges of the sequence
#' @return A matrix with dimensions length(x) - 1 by 2 or
#' length(x) + 1 by 2 if do.pad == TRUE.
#'
#' @author Brian Lee Yung Rowe
#' @seealso \code{\link{partition}} \code{\link{maprange}}
#' @note The segment function is a convenience and can be implemented
#' using the general functions partition and also maprange. If you want
#' more than two columns, use maprange.
#'
#' @examples
#' segment(1:10)
#'
#' # Notice how the ends of the sequence are given their own rows
#' segment(1:10, TRUE)
#'
#' # Emulate segment using partition
#' partition(1:10, function(x) x, 1)
#'
#' # Emulate segment using maprange
#' t(maprange(1:10, 2, function(x) x))
#'
#' # Create four shifted copies instead of two
#' maprange(1:10, 4, function(x) x)
segment(x, do.pad=FALSE) %when% {
  is.null(dim(x))
} %as% {
  x <- onlyif(do.pad, function(y) pad(y,1,1), x)
  m <- matrix(c(x[1:(length(x)-1)], x[2:length(x)]), ncol=2)
  colnames(m) <- c('a','b')
  m
}

#' Safely get an element from a vector
#'
#' This function guarantees a vector of length > 1 as the return value of
#' an indexing operation.
#'
#' @section Usage:
#' item(v, idx)
#'
#' @section Details:
#' Standard R indexing yields different results depending on the input.
#' When either an empty vector or a NULL is passed to the indexing
#' operator, an empty vector is returned. However, if the index is NA,
#' the return value will be a vector of NAs having the same length as
#' the original vector. This inconsistent behavior requires special
#' handling whenever the index value is computed dynamically.
#'
#' This function is designed to create a consistent return value for a 
#' bad index value, which is defined as {NULL, NA, vector of length 0}.
#' If any of these values are used as the index, then NA is returned
#' instead of an empty vector.
#'
#' @name item
#' @param v A sequence
#' @param idx The index of the element to extract
#' @return Either the value of x[idx] or NA for invalid index values
#' 
#' @author Brian Lee Yung Rowe
#'
#' @examples
#' # Compare default behavior with item
#' (1:10)[NA]
#' item(1:10, NA)
#'
#' # Negative indices are still allowed
#' item(1:10, -2) 
item(v, NULL) %as% NA
item(v, NA) %as% NA
item(v, EMPTY) %as% NA
item(v, 0) %as% NA
item(v, idx) %when% { is.null(dim(v)) } %as% v[idx]

#' Find contiguous ranges of a given value within a sequence
#'
#' Identify the index ranges for a given value in a sequence and return
#' the minimum and maximum values of the ranges.
#'
#' @section Usage:
#' range.for(target, x)
#'
#' @name range.for
#' @param target A value to find in x
#' @param x A vector
#' @return A data.frame where each row specifies the end points
#' of a contiguous range that contains the target value
#'
#' @author Brian Lee Yung Rowe
#'
#' @examples
#' # Find all contiguous ranges containing 2
#' x <- sample(c(1,2,2,2,3,4), 20, replace=TRUE)
#' range.for(2,x) 
#'
range.for(target, x) %when% {
  is.null(dim(x))
} %as% {
  y <- segment(x, TRUE)
  a <- y[,'a']
  b <- y[,'b']
  idx <- 1:nrow(y)
  idx.inf <- (is.na(a) | a != target) & (!is.na(b) & b == target)
  idx.sup <- (!is.na(a) & a == target) & (is.na(b) | b != target)
  data.frame(min=idx[idx.inf], max=idx[idx.sup]-1)
}

#' Sample sub-sequences from a sequence
#'
#' This is like the normal sample function but instead of a scalar,
#' vector sub-sequences are extracted from the input.
#'
#' @section Usage:
#' samplerange(x, size, window, ...)
#'
#' @section Details:
#' Sometimes a sequence is auto-correlated. Attempting to construct a
#' a sub-sequence by sampling from such a sequence will lose the 
#' auto-correlation embedded within the original sequence. The solution
#' is to draw random sub-sequences from the original sequence, which
#' is what this function does.
#'
#' This operation can be for both a sequence (i.e. a vector or array)
#' or a matrix/data.frame. If the latter, a sub-matrix is selected 
#' such that the columns of the matrix are preserved.
#' This behavior is consistent
#' with time series data formats where a single series is represented
#' by a column and each row represents a point in time. Hence, the
#' 2D version will select sub-sequences in time, collecting all
#' associated time series.
#'
#' Under the hood, this function relies on sample.int, so the behavior
#' of the output can be controlled by passing additional arguments to
#' sample.int, such as replace=TRUE.
#'
#' @name samplerange
#' @param x A one-dimensional or two-dimensional data structure
#' @param size The number of sub-sequences to create
#' @param window The length of the output vectors 
#' @param \dots Optional arguments for the sample.int function
#'
#' @return 
#' When a sequence is passed to samplerange a matrix is returned,
#' where each column represents a sampled subsequence. Hence the
#' dimensions of the matrix will be window by size.
#'
#' If a matrix is passed to samplerange then a list of sub-matrices
#' is returned. Each sub-matrix will be of dimension window by ncol(x).
#' The length of the resulting list will be size.
#'
#' In either case, each _column_ is independent.
#'
#' @author Brian Lee Yung Rowe
#'
#' @examples
#' # Extract seven sub-sequences, each with length 3
#' samplerange(1:20, 7, 3)
#'
#' # This time use replacement
#' samplerange(1:20, 7, 3, replace=TRUE)
#'
#' # Extract five sub-matrices with dimensions 2 by 4
#' samplerange(matrix(1:32, ncol=4), 5, 2)
samplerange(x, size, window, ...) %when% {
  is.null(dim(x))
  window < length(x)
} %as% {
  count <- length(x) - window + 1
  samples <- sample.int(count, size, ...)
  sapply(samples, function(s) x[s:(s+window-1)])
}

samplerange(x, size, window, ...) %when% {
  window < length(x)
} %as% {
  count <- nrow(x) - window + 1
  samples <- sample.int(count, size, ...)
  lapply(samples, function(s) x[s:(s+window-1),])
}
