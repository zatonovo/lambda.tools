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
#' @examples
#' map(rnorm(10, sd=2), quantize)
#'
map(EMPTY, fn, y) %as% y

map(x, fn, y=c()) %when% { is.null(dim(x)) } %as%
  map(x[-1], fn, c(y, fn(x[[1]])))

map(x, fn, y=c()) %as% map(x[,-1,drop=FALSE], fn, c(y, fn(x[,1])))


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
#' @name maprange
#'
#' @examples
#' x <- rnorm(50)
#' x10 <- maprange(x, 10, mean, TRUE)
#' x20 <- maprange(x, 20, mean)
maprange(x, window, fn, do.pad=FALSE) %when% {
  is.null(dim(x))
} %as% {
  y <- sapply(window:length(x), function(idx) fn(x[(idx-window+1):idx]))
  onlyif(do.pad, function(z) pad(z, window-1), y)
}

maprange(x, window, fn, do.pad=FALSE) %as% {
  sapply(1:ncol(x), function(ydx) maprange(x[,ydx], fn, window, do.pad))
}


#' Successively apply a function to a sequence and the value of the
#' previous application
#'
#' @name fold
#'
#' @examples
#' fold(rnorm(10), function(x,y) x+y)
#'
# Things to test
# . Vector
# . List
# . Matrix
# . Data.frame
# . Vector of length 1
fold(EMPTY, fn, acc) %as% acc

fold(x, fn, acc=0) %when% { is.null(dim(x)) } %as% 
  fold(x[-1], fn, fn(x[[1]], acc))

fold(x, fn, acc=0) %as% fold(x[,-1,drop=FALSE], fn, fn(x[,1], acc))


#' Perform fold over a window
#'
#' @name foldrange
#' @param x
#' @param window
#' @param fn
#' @param acc
foldrange(x, window, fn, acc=0) %when% {
  is.null(dim(x))
} %as% {
  foldrange(x, window, fn, acc, length(x)-window+1)
}

foldrange(x, window, fn, acc, 0) %as% acc

foldrange(x, window, fn, acc=0, idx) %as% {
  foldrange(x, window, fn, fn(x[idx:(idx+window-1)], acc), idx-1)
}

