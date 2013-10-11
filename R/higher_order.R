# :vim set filetype=R
#' Apply a function over each element of a vector
#'
#' This function implements a map operation over different data types. This provides
#' polymorphism for vectors, lists, matrices and data.frames.
#'
#' @section Usage:
#' map(x, fn, y=c())
#'
#' @name map
#' @param x Any indexable data structure
#' @param fn A function applied to elements in x
#' @param y An accumulator object (vector, list, matrix, data.frame)
#'
#' @section Details:
#' This function is implemented using recursion and will throw an error if the  
#' length of \code{x} approaches \code{getOption('expressions') / 8.0}. This limit
#' is due to R session protecting against infinite recursion via the
#' expressions parameter. See \code{options}.
#'
#' Recursion will decrement the length of the input object \code{x} and eventually the
#' above function clause will be called as a result of \code{x} being empty. At that point
#' \code{map} will return the accumulator \code{y}. 
#'
#' @return The value returned is the accumulator object \code{y} 
#'
#' @examples
#' map(-10:10, quantize)
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
#' This function applies a function over a rolling range of a vector.
#'
#' @name maprange
#' @param x A vector
#' @param window Number of elements included in rolling range
#' @param fn A function applied to the rolling range in x
#'
#' @section Usage:
#' maprange(x, window, fn, do.pad=FALSE)
#' 
#' @section Details:
#' This function is implemented using recursion and will throw an error if the  
#' length of \code{x} approaches \code{getOption('expressions') / 8.0}. This limit
#' is due to R session protecting against infinite recursion via the
#' expressions parameter. See \code{options}.
#'
#' @return a vector containing the result of fn applied to the rolling window.
#'
#' @examples
#' x <- 1:10
#' x3 <- maprange(x, 3, sum, TRUE)
#'
#' # Notice the difference in output vector when do.pad is FALSE.
#' x2 <- maprange(x, 2, sum)
#' x3 <- maprange(x, 3, sum)
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

#' Apply a function over blocks of a vector
#'
#' Apply a function over blocks of a data structure. This function implements 
#' polymorphism over lists, vectors, matrices and data.frames.
#'
#' @name mapblock
#' @param x Any indexable data structure
#' @param block The block size used to map over
#' @param fn A function applied to a block
#'
#' @section Usage:
#' mapblock(x, block, fn, do.pad=FALSE)
#'
#' @section Details:
#' The function \code{fn} must take one required argument. If the block size is not 
#' a multiple of \code{anylength(x)} the vector returned will have \code{NA}s for indices
#' that fall outside the range of the blocks.
#'
#' For two-dimensional structures, blocking occurs across the columns of the data
#' structure and the function will be applied to all rows for that given block of columns.
#'
#' @return A vector containing the result of fn applied to each block 
#'
#' @examples
#'  
#' x <- 1:10
#' # Apply mean to blocks of x - look how the length of output excluding NAs is 
#' # length(x) / block.
#'
#' x2 <- mapblock(x, 2, mean)
#' x2 <- mapblock(x, 2, mean, TRUE)
#' x5 <- mapblock(x, 5, mean)
#'
#' # Apply mapblock across the columns of a matrix for two block sizes - Note
#' # how the function is applied to block columns.
#' 
#' m <- matrix(1:12, ncol=2)
#' mapblock(m, 1, sum)
#' mapblock(m, 2, sum)
#'
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
#' This function applies a function to a sequence and the value of the previous 
#' application, see references.
#'
#' @name fold
#' @param x Any indexable data structure
#' @param fn A function applied to x
#' @param acc Accumulator
#'
#' @section Usage:
#' fold(x, fn, acc=0)
#'
#' @section Details:
#' This function implements a linear fold operation via recursion. The reduction process
#' is accomplished by recursively passing x[-1] into the inner function call. For each 
#' call to fold, the input vector is shrinking by one element and the function applied
#' to the data is applied to the first element of the input vector and the accumulator. 
#' Hence, the function applied to the blocks must take two arguments
#' (i.e., a binary function).
#'
#' @references Haskell Wiki, http://www.haskell.org/haskellwiki/Fold
#' @references Brian Lee Yung Rowe, Modeling Data with Functional Programming in R.
#'
#' @return An object containing the accumulated result.
#'
#' @examples
#' x <- 1:10
#' fold(x, function(a,b) a+b)
#' fold(x, function(a,b) a+b, acc=10)
#'
#' x <- list(1:10)
#' fold(x[[1]], function(a,b) a+b)
#'
#' # Fold across the columns of a matrix.
#' x <- matrix(1:10, ncol=2)
#' fold(x, function(a,b) a+b)
#'
#' # Fold across the rows of a data.frame.
#' x <- data.frame(col1=1:10, col2=1:10)
#' fold(x, function(a,b) a+b)
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
#' This function successively applies a function to a sequence and the value
#' of the previous application over a rolling range of a vector.
#'
#' @name foldrange
#' @param x A vector
#' @param window The number of elements included in the rolling range 
#' @param fn The function applied to the rolling range 
#' @param acc Accumulator
#'
#' @section Usage:
#' foldrange(x, window, fn, acc=0)
#'
#' @section Details:
#' This function implements a linear fold operation over a rolling range with
#' length defined by the window parameter. This function is defined for one- and 
#' two dimensional data structures only.  A restriction on the window size is that 
#' the window size must be less than the \code{length(x)}. The function applied to the 
#' window must take two arguments (i.e., a binary function).
#'
#' @return An object containing the accumulated result
#'
#' @examples
#' x <- 1:10
#' foldrange(x, 3, function(a,b) a+b)
#'
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
#' This function applies a function to a moving block of a sequence and the previous 
#' application of the function. 
#'
#' @name foldblock
#' @param x Any Indexable data structure
#' @param block The number of elements included in the rolling block
#' @param fn The function applied to the rolling range
#' @param acc Accumulator
#'
#' @section Usage:
#' foldblock(x, block, fn, acc=0)
#'
#' @return An object containing the accumulated result
#'
#' @section Details:
#' This function apples to both one-dimensional and two-dimensional data structures.
#' A restriction on the block size is that the block size must be less than the length(x).
#' The function applied to the blocks must take two arguments (i.e., a binary function).
#'
#' @section TODO: 
#' This function is not working for a matrix. See github issue
#' https://github.com/muxspace/lambda.tools/issues/3
#'
#' @examples
#' x <- 1:10
#' foldblock(x, 2, function(a,b) a+b)
#'
#' # fold with a block size of 3 - Notice how the length of the output changes.
#' foldblock(x, 3, function(a,b) a+b)
#' 
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
