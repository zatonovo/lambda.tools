# :vim set filetype=R

#' Apply a function over each element of a vector
#'
#' @name map
#' @param x Any indexable data structure.
#' @param fn A function applied to elements in x. 
#' @param y An accumulator object (vector, list, matrix, data.frame)
#'
#' @section Details:
#' This function is implemented using recursion and will throw an error if the  
#' length of \code{x} approaches \code{getOption('expressions') / 8.0}. This limit
#' is due to R session protecting against infinite recursion via the
#' expressions parameter. See \code{options}.
#'
#' @return The value returned is the accumulator object \code{y} which is 
#' returned from the first function clause (run \code{describe(map, 1)} in the R session).
#'
#' Recursion will decrement the length of the input object \code{x} and eventually the
#' above function clause will be called as a result of \code{x} being empty. At that point
#' \code{map} will return the accumulator \code{y}. 
#'
#' @examples
#' map(rnorm(10, sd=2), quantize)
#'
#' map(rnorm(10, sd=2), function(y) sum(y), y=list())
#'
#' # Sum the columns of a matrix
#' map(matrix(rnorm(10, sd=2), ncol=2), function(y) sum(y), y=list())
#'
#' # Sum the columns of a data.frame 
#' map(matrix(rnorm(10, sd=2), ncol=2), function(y) sum(y), y=list())
#
map(EMPTY, fn, y) %as% y

map(x, fn, y=c()) %when% { 
    is.null(dim(x)) 
    is.function(fn)
} %as% {
  map(x[-1], fn, c(y, fn(x[[1]])))
}

map(x, fn, y=c()) %when% {
  is.function(fn)    
} %as% {
  map(x[,-1,drop=FALSE], fn, c(y, fn(x[,1])))
}

#' Apply a function over a rolling range of a vector
#'
#' @name maprange
#' @param x Any indexable data structure.
#' @param window Number of elements included in rolling range.
#' @param fn A function applied to the rolling range in x.
#'
#' @section Details:
#' This function is implemented using recursion and will throw an error if the  
#' length of \code{x} approaches \code{getOption('expressions') / 8.0}. This limit
#' is due to R session protecting against infinite recursion via the
#' expressions parameter. See \code{options}.
#'
#' @return a vector containing the result of fn appled to the rolling window.
#'
#' @examples
#' x <- rnorm(50)
#' x10 <- maprange(x, 10, mean, TRUE)
#'
#' x20 <- maprange(x, 20, mean)
maprange(x, window, fn, do.pad=FALSE) %when% {
  is.null(dim(x))
  window < anylength(x)
} %as% {
  y <- sapply(window:length(x), function(idx) fn(x[(idx-window+1):idx]))
  onlyif(do.pad, function(z) pad(z, window-1), y)
}

maprange(x, window, fn, do.pad=FALSE) %when% {
  window < anylength(x)
} %as% {
  sapply(1:ncol(x), function(ydx) maprange(x[,ydx], fn, window, do.pad))
}

# Things to test:
# . Vector
# . List
# . Matrix
# . Data.frame
# . Vector of length 1
# . When window > length(x)
#
#' Apply a function over blocks of a vector
#'
#' @name mapblock
#' @param x Any indexable data structure.
#' @param block The block size used to map over. 
#' @param fn A function applied to a block.
#'
#' @section Details:
#' The function used must take one required argment. If the block size is not 
#' a multiple of \code{anylength(x)} the vector returned will have NAs for indices
#' that fall outside the range of the blocks.
#'
#' @return a vector containing the result of fn appled to the rolling window.
#'
#' @examples
#' x <- rnorm(50)
#' x10 <- mapblock(x, 10, mean, TRUE)
#'
#' x20 <- mapblock(x, 20, mean)
mapblock(x, block, fn, do.pad=FALSE) %when% {
  is.null(dim(x))
  block < anylength(x)
} %as% {
  y <- sapply(seq(1, length(x), by=block), function(idx) fn(x[idx:(idx+block-1)]))
  onlyif(do.pad, function(z) pad(z, block-1), y)
}

mapblock(x, block, fn, do.pad=FALSE) %when% {
  block < anylength(x)
} %as% {
  y <- sapply(seq(1, ncol(x), by=block), function(ydx) fn(x[,ydx:(ydx+block-1)]))
  onlyif(do.pad, function(z) pad(z, block-1), y)
}

#' Successively apply a function to a sequence and the value of the
#' previous application
#'
#' @name fold
#' @param x Any indexable data structure.
#' @param fn a function applied to x
#' @param acc accumulator
#'
#' @section Details:
#' This function implements a linear fold operation as opposed to a tree-like fold. 
#' The function applied to the blocks must take two arguments (i.e., a binary function).
#'
#' @return An object containing the accumulated result.
#'
#' @examples
#' fold(rnorm(10), function(x, y) x + y)
#'
#' fold(rnorm(10), function(x, y) x + y, acc=10)
#'
#' # Fold over a list element.
#' x <- list(1:10)
#' fold(x[[1]], function(x, y) x + y)
#'
#' # Fold across the rows of a matrix.
#' x <- matrix(1:10, ncol=2)
#' fold(x, function(x, y) x + y)
#'
#' # Fold accross the rows of a data.frame.
#' x <- data.frame(x1=1:10, x2=1:10)
#' fold(x, function(x, y) x + y)
fold(EMPTY, fn, acc) %as% acc

fold(x, fn, acc=0) %when% { 
  is.null(dim(x)) 
  is.function(fn)
} %as% {
  fold(x[-1], fn, fn(x[[1]], acc))
}

fold(x, fn, acc=0) %when% {
  is.function(fn)   
} %as% { 
  fold(x[,-1,drop=FALSE], fn, fn(x[,1], acc))
}

#' Successively apply a function to a sequence and the value of the
#' previous application over a rolling range of a vector
#'
#' @name foldrange
#' @param x Any indexable data structure.
#' @param window the number of elements included in the rolling range 
#' @param fn the function applied to the rolling range 
#' @param acc accumulator
#'
#' @section Details:
#' This function implements a linear fold operation over a rolling range with
#' length defined by the window parameter. This function is defined for one- and 
#' two dimensional data structures only.  A restriction on the window size is that 
#' the window size must be less than the length(x).  The function applied to the 
#' window must take two arguments (i.e., a binary function).
#'
#' @return An object containing the accumulated result.
#'
#' @examples
#' foldrange(rnorm(10), 2, function(x,y) x + y)
#'
# Things to test
# . Vector
# . List
# . Matrix
# . Data.frame
# . Vector of length 1
foldrange(x, window, fn, acc=0) %when% {
  is.null(dim(x))
  window < anylength(x)
} %as% {
  foldrange(x, window, fn, acc, length(x)-window+1)
}

foldrange(x, window, fn, acc, 0) %as% acc

foldrange(x, window, fn, acc=0, idx) %when% {
  window < anylength(x)
} %as% {
  foldrange(x, window, fn, fn(x[idx:(idx+window-1)], acc), idx-1)
}

#' Successively apply a function to a block of a sequence and the 
#' value of the previous application over a moving block subsequence of 
#' a vector
#'
#' @name foldblock
#' @param x Any indexable data structure
#' @param block the number of elements included in the rolling block
#' @param fn the function applied to the rolling range
#' @param acc accumulator
#'
#' @section Details:
#' This function apples to both one-dimensional and two-dimensional data structures.
#' A restriction on the block size is that the block size must be less than the length(x).
#' The function applied to the blocks must take two arguments (i.e., a binary function).
#'
#' @examples
#' foldblock(rnorm(10), 2, function(x,y) x + y)
foldblock(x, block, fn, acc=0) %when% { 
  is.null(dim(x))
  block < anylength(x)
} %as% {
  foldblock(x, block, fn, acc, length(x)-block+1)
}

foldblock(x, block, fn, acc=0) %when% { 
  block < anylength(x)
} %as% {
  foldblock(x, block, fn, acc, ncol(x)-block+1)
}

foldblock(x, block, fn, acc, idx) %when% {
  idx <= 0
} %as% { acc }

foldblock(x, block, fn, acc=0, idx) %when% {
  is.null(dim(x))
  block < anylength(x)
} %as% {
  foldblock(x, block, fn, fn(x[idx:(idx+block-1)], acc), idx-block)
}

foldblock(x, block, fn, acc=0, idx) %when% { 
  block < anylength(x)
} %as% {
  foldblock(x, block, fn, fn(x[,idx:(idx+block-1)], acc), idx-block)
}
