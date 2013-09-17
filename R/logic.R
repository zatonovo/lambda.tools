# :vim set filetype=R

#' Conditionally apply a function to an argument
#'
#' @name onlyif
#' @param fn
#' @param x
#'
#' @examples
#' x <- rnorm(5)
#' onlyif(length(x) < 10, function(y) pad(y, 10 - length(y)), x)
onlyif(TRUE, fn, x) %as% f(x)
onlyif(FALSE, fn, x) %as% x

#' Apply a default value whenever a variable is empty, NULL, or NA
#'
#' @name use_default
#' @param x 
#' @param default
#'
#' @examples
#' x <- sample(c(1:3,NA), 10, replace=TRUE)
#' map(x, function(y) use_default(y,0))
use_default(EMPTY, default) %as% default
use_default(NULL, default) %as% default
use_default(NA, default) %as% default
use_default(x, default) %as% x


