# :vim set filetype=R

# Things to test:
# . Vector
# . List
# . Matrix
# . Data.frame
#
#' Apply a function over each element of a vector
#'
#' @name map
#' @param x a data structure containing elements that are compatable with fn
#' @param fn a function 
#' @param y an accumulator object (vector, list, matrix, data.frame)
#'
#' @section Details:
#' This function is implemented using recursion and will throw an error if the  
#' length of \code{x} approaches \code{getOption('expressions') / 8.0}. This limit
#' is due to R session protecting against infinite recursion via the
#' expressions parameter. See \code{options}.
#'
#' @section Value:
#' The value returned is the accumulator object \code{y} which is returned from the first 
#' function clause (run \code{describe(map, 1)} in the R session).
#' 
#'
#' Recursion will decrement the length of the input object \code{x} and eventually the
#' above function clause will be called as a result of \code{x} being empty. At that point
#' \code{map} will return the accumulator \code{y}. 
#'
#' @examples
#' map(rnorm(10, sd=2), quantize)
#' map(rnorm(10, sd=2), function(y) sum(y), y=list())
#' # Sum the columns of a matrix
#' map(matrix(rnorm(10, sd=2), ncol=2), function(y) sum(y), y=list())
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

# Things to test:
# . Vector
# . List
# . Matrix
# . Data.frame
# . Vector of length 1
# . When window > length(x)
#
#' Apply a function over a rolling range of a vector
#'
#' @name rangemap
#' @param x a vector, list, matrix, or data.frame 
#' @param window number of elements included in rolling range
#' @param fn a function applied to the rolling range in x 
#' @examples
#' x <- rnorm(50)
#' x10 <- rangemap(x, 10, mean, TRUE)
#' x20 <- rangemap(x, 20, mean)
rangemap(x, window, fn, do.pad=FALSE) %when% {
  is.null(dim(x))
  window < anylength(x)
} %as% {
  y <- sapply(window:length(x), function(idx) fn(x[(idx-window+1):idx]))
  onlyif(do.pad, function(z) pad(z, window-1), y)
}

rangemap(x, window, fn, do.pad=FALSE) %when% {
  window < anylength(x)
} %as% {
  sapply(1:ncol(x), function(ydx) rangemap(x[,ydx], fn, window, do.pad))
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
# Document this:
# x <- 1:10
# > blockmap(x, 3, function(a) sum(a))
# [1]  6 15 24 NA
#' @name blockmap
#' @param x a vector, list, matrix, or data.frame 
#' @param block the block size used to map over 
#' @param fn a function applied to a block
#' @examples
#' x <- rnorm(50)
#' x10 <- blockmap(x, 10, mean, TRUE)
#' x20 <- blockmap(x, 20, mean)
blockmap(x, block, fn, do.pad=FALSE) %when% {
  is.null(dim(x))
  block < anylength(x)
} %as% {
  y <- sapply(seq(1, length(x), by=block), function(idx) fn(x[idx:(idx+block-1)]))
  onlyif(do.pad, function(z) pad(z, block-1), y)
}

blockmap(x, block, fn, do.pad=FALSE) %when% {
  block < anylength(x)
} %as% {
  y <- sapply(seq(1, ncol(x), by=block), function(ydx) fn(x[,ydx:(ydx+block-1)]))
  onlyif(do.pad, function(z) pad(z, block-1), y)
}

#' Successively apply a function to a sequence and the value of the
#' previous application
#'
#' @name fold
#' @param x a vector, list, matrix or data.frame
#' @param fn a function applied to x
#' @param acc accumulator
#'
#' @examples
#' fold(rnorm(10), function(x, y) x + y)
#' fold(rnorm(10), function(x, y) x + y, acc=10)
#' # Fold over a list element.
#' x <- list(1:10)
#' fold(x[[1]], function(x, y) x + y)
#' # Fold across the rows of a matrix.
#' x <- matrix(1:10, ncol=2)
#' fold(x, function(x, y) x + y)
#' # Fold accross the rows of a data.frame.
#' x <- data.frame(x1=1:10, x2=1:10)
#' fold(x, function(x, y) x + y)
#'
#'
# Things to test
# . Vector
# . List
# . Matrix
# . Data.frame
# . Vector of length 1
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
#' @name rangefold
#' @param x a vector, lsit, matrix or data.frame
#' @param window the number of elements included in the rolling range 
#' @param fn the function applied to the rolling range 
#' @param acc accumulator
#' @param idx starting index for rolling range
#'
#' @examples
#' rangefold(rnorm(10), 2, function(x,y) x + y)
#'
# Things to test
# . Vector
# . List
# . Matrix
# . Data.frame
# . Vector of length 1
rangefold(x, window, fn, acc=0) %when% {
  is.null(dim(x))
  window < anylength(x)
} %as% {
  rangefold(x, window, fn, acc, length(x)-window+1)
}

rangefold(x, window, fn, acc, 0) %as% acc

rangefold(x, window, fn, acc=0, idx) %when% {
  window < anylength(x)
} %as% {
  rangefold(x, window, fn, fn(x[idx:(idx+window-1)], acc), idx-1)
}

#' Successively apply a function to a block of a sequence and the 
#' value of the previous application over a moving block subsequence of 
#' a vector
#'
#' @name blockfold
#'
blockfold(x, block, fn, acc=0) %when% { 
  is.null(dim(x))
  block < anylength(x)
} %as% {
  blockfold(x, block, fn, acc, length(x)-block+1)
}

blockfold(x, block, fn, acc=0) %when% { 
  block < anylength(x)
} %as% {
  blockfold(x, block, fn, acc, ncol(x)-block+1)
}

blockfold(x, block, fn, acc, idx) %when% {
  idx <= 0
} %as% { acc }

blockfold(x, block, fn, acc=0, idx) %when% {
  is.null(dim(x))
  block < anylength(x)
} %as% {
  blockfold(x, block, fn, fn(x[idx:(idx+block-1)], acc), idx-block)
}

blockfold(x, block, fn, acc=0, idx) %when% { 
  block < anylength(x)
} %as% {
  blockfold(x, block, fn, fn(x[,idx:(idx+block-1)], acc), idx-block)
}
