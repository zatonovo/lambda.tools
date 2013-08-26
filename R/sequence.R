# :vim set filetype=R

#' Pad a vector with NAs
pad(x, head, tail=0, default=NA) %as% c(rep(default,head),x, rep(default,tail))

#' Safely get an element from a vector
#'
#' Returns NA whenever a bad index is encountered
item(v, NA) %as% NA
item(v, idx) %when% { length(idx) == 0 } %as% NA
item(v, idx) %as% v[idx]

range.for(value, series) %as% {
  x <- segment(series, TRUE)
  idx <- 1:nrow(x)
  idx.inf <- (is.na(x$a) | x$a != value) & (!is.na(x$b) & x$b == value)
  idx.sup <- (!is.na(x$a) & x$a == value) & (is.na(x$b) | x$b != value)
  data.frame(min=idx[idx.inf], max=idx[idx.sup]-1)
}

#' Get a sample as a subsequence of a larger set
samplerange(x, size, window, ...) %when% {
  is.null(dim(x))
  window < length(x)
} %as% {
  count <- length(x) - window + 1
  samples <- sample.int(count, size, ...)
  t(sapply(samples, function(s) x[s:(s+window-1)]))
}

#' @return A list containing each window
#' 
samplerange(x, size, window, ...) %when% {
  window < length(x)
} %as% {
  count <- nrow(x) - window + 1
  samples <- sample.int(count, size, ...)
  lapply(samples, function(s) x[s:(s+window-1),])
}


