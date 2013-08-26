# :vim set filetype=R

map(fn, EMPTY, y) %as% y

map(fn, x, y=c()) %as% map(fn, x[-1], c(y, fn(x[[1]])))



#' @examples
#' x10 <- maprange(x, mean, 10, TRUE)
#' x20 <- maprange(x, mean, 20, TRUE)
maprange(x, fn, window, pad=FALSE) %when% {
  is.null(dim(x))
} %as% {
  y <- sapply((window):length(x), function(idx) fn(x[(idx-window+1):idx]))
  onlyif(function(z) pad(z, window-1), y, pad)
}

maprange(x, fn, window, pad=FALSE) %as% {
  sapply(1:ncol(x), function(ydx) maprange(x[,ydx], fn, window, pad))
}


fold(f, EMPTY , acc) %as% acc

fold(f, x, acc) %when% { is.null(dim(x)) } %as% fold(f, x[-1], f(x[[1]], acc))

fold(f, x, acc) %as% fold(f, x[,-1,drop=FALSE], f(x[,1], acc))


foldrange() %as% {

}
