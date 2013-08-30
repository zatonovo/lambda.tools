# :vim set filetype=R

#' Apply a function over each element of a vector
#'
#' @examples
#' map(rnorm(10, sd=2), bin)
# Things to test:
# . Vector
# . List
# . Matrix
# . Data.frame
map(fn, EMPTY, y) %as% y

map(fn, x, y=c()) %when% { is.null(dim(x)) } %as%
  map(fn, x[-1], c(y, fn(x[[1]])))

map(fn, x, y=c()) %as% map(fn, x[,-1,drop=FALSE], c(y, fn(x[,1])))


#' Apply a function over a rolling range of a vector
#'
#'
#' @examples
#' x10 <- maprange(x, mean, 10, TRUE)
#' x20 <- maprange(x, mean, 20, TRUE)
# Things to test:
# . Vector
# . List
# . Matrix
# . Data.frame
# . Vector of length 1
# . When window > length(x)
maprange(fn, x, window, pad=FALSE) %when% {
  is.null(dim(x))
} %as% {
  y <- sapply((window):length(x), function(idx) fn(x[(idx-window+1):idx]))
  onlyif(function(z) pad(z, window-1), y, pad)
}

maprange(fn, x, window, pad=FALSE) %as% {
  sapply(1:ncol(x), function(ydx) maprange(fn, x[,ydx], window, pad))
}


#' Successively apply a function to a sequence and the value of the
#' previous application
#'
#' @examples
#'
# Things to test
# . Vector
# . List
# . Matrix
# . Data.frame
# . Vector of length 1
fold(fn, EMPTY, acc) %as% acc

fold(fn, x, acc) %when% { is.null(dim(x)) } %as% 
  fold(fn, x[-1], f(x[[1]], acc))

fold(fn, x, acc) %as% fold(fn, x[,-1,drop=FALSE], f(x[,1], acc))


# Not sure if there's a need for this
#foldrange(x, fn, window, pad=FALSE) %when% {
#  is.null(dim(x))
#} %as% {
#  y <- sapply((window):length(x), function(idx) fn(x[(idx-window+1):idx]))
#  onlyif(function(z) pad(z, window-1), y, pad)
#}
