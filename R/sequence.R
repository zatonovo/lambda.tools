# :vim set filetype=R

#' Pad a vector with NAs
#'
#' @name pad
#' @param x
#' @param head
#' @param tail
#' @param default
pad(x, head, tail=0, default=NA) %as% c(rep(default,head),x, rep(default,tail))

#' Partition a sequence into coordinate pairs based on adjacent windows
#'
#' @name partition
#' @param x
#' @param metric
#' @param radius
partition(x, metric=median, radius=10) %as% {
  f <- function(x,i) {
    c(left=metric(x[max(1,i-radius):i]), 
      right=metric(x[(i+1):min(length(x),i+1+radius)]))
  }
  t(sapply(1:(length(x)-1), function(i) f(x,i)))
}


#' Similar to partion where radius=2 and there is no metric
#'
#' @name segment
#' @param x
#' @param pad
segment(x, pad=FALSE) %as% {
  x <- onlyif(pad, function(y) pad(y,1,1), x)
  data.frame(a=x[1:(length(x)-1)], b=x[2:length(x)])
}

#' Safely get an element from a vector
#'
#' Returns NA whenever a bad index is encountered
#'
#' @name item
item(v, NA) %as% NA
item(v, idx) %when% { length(idx) == 0 } %as% NA
item(v, idx) %as% v[idx]


#' Get the range of a value in a series
#'
#' @name range.for
#' @param value
#' @param series
range.for(value, series) %as% {
  x <- segment(series, TRUE)
  idx <- 1:nrow(x)
  idx.inf <- (is.na(x$a) | x$a != value) & (!is.na(x$b) & x$b == value)
  idx.sup <- (!is.na(x$a) & x$a == value) & (is.na(x$b) | x$b != value)
  data.frame(min=idx[idx.inf], max=idx[idx.sup]-1)
}

#' Get a sample as a subsequence of a larger set
#'
#' @name samplerange
#' @param x
#' @param size
#' @param window
#' @param \dots
#' @return A matrix 
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


