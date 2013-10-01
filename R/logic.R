# :vim set filetype=R

#' Check if an object is a scalar
#'
#' @name is.scalar
#' @param x an object
#' 
#' @section Details:
#' This function checks to determine if an object \code{x} is a scalar. The usage of 
#' 'scalar' in this function defintion is intened to mean any object with length equal
#' to one.
#'
#' @examples
#' x <- 10
#' is.scalar(x)
is.scalar(x) %when% {
  length(x) == 1
} %as% { TRUE }
is.scalar(x) %as% { FALSE }

#' Conditionally apply a function to an argument
#'
#' @name onlyif
#' @param condition logical statement used to conditionally apply fn to x
#' @param fn a function to apply to x
#' @param x an object
#'
#' @section Details:
#' This function can be used to apply a function to a vector containing
#' elements that lie outside the valid domain of \code{fn}. Th function \code{onlyif}
#' differs from \code{ifelse} in the sense that it is not vectorized and a closure
#' can be used. For example,
#'
#' \code{ifelse(length(x) < 10, function(y) fold(x, function(x, y) x+y), x)}.
#'
#' will fail due to the closure around \code{fold}.  If the argument \code{fn}, is not
#' a function \code{onlyif} will throw an error. 
#'
#' @examples
#' x <- rnorm(5)
#' onlyif(length(x) < 10, function(y) pad(y, 10 - length(y)), x)
#' onlyif(length(x) < 10, function(y) fold(x, function(x, y) x+y), x)
onlyif(TRUE, fn, x) %when% { 
  is.function(fn)
} %as% { fn(x) }
onlyif(FALSE, fn, x) %as% { x }

#' Apply a default value whenever a variable is empty, NULL, or NA
#'
#' @name use_default
#' @param x a scalar variable
#' @param default the value to replace empty, NULL, or NA
#'
#' @section Details:
#' These are equivlaent operations,  
#'
#' \code{x <- sample(c(1:3, NA), 10, replace=TRUE)}
#' \code{x[is.na(x)] <- 0}
#' \code{map(x, function(y) use_default(y,0))}
#'
#' The value in using \code{use_default} instead of element replacement by a set 
#' operation is that the functional definition will help mathematical provability 
#' of the program and facilitate translation between the mathematical model and code.
#'
#' @examples
#' # Clean data.
#' x <- sample(c(1:3,NA), 10, replace=TRUE)
#' map(x, function(y) use_default(y, 0))
use_default(EMPTY, default) %as% default
use_default(NULL, default) %as% default
use_default(NA, default) %as% default
use_default(x, default) %when% { is.scalar(x) } %as% x
