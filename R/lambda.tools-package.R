#' Tools for functional programming in R
#'
#' This package contains a collection of utility functions that provide 
#' polymorphism over certain data types. These any* functions 
#' attempt to consolidate attribute access of lists, vectors, matrices, arrays,
#' and other data structures.
#'
#' \tabular{ll}{
#' Package: \tab lambda.tools\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0.0\cr
#' Date: \tab 2013-08-30\cr
#' License: \tab LGPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' The anylength and anynames functions consolidate attribute access across many
#' data structures providing a bit of convenience via polymorphism. The anytypes
#' function provides the classes or types of a data.frame-like object. This is
#' useful when parsing data and it is not always clear how values will be 
#' parsed.
#'
#' @name lambda.tools-package
#' @aliases lambda.tools-package lambda.tools
#' @docType package
#' @exportPattern "^[^\\.]"
#' @author Brian Lee Yung Rowe <r@@zatonovo.com>
#' @seealso \code{\link{anylength}}, \code{\link{anynames}}, \code{\link{anytypes}}
#' @keywords package attribute logic
#' @examples
#' m <- matrix(c(1,2,3,4,5,6), ncol=2)
#' anylength(m)
#'
#' v <- c(1,2,3,4,5)
#' anylength(v)
#'
#' m <- matrix(c(1,2,3,4,5,6), ncol=2)
#' anynames(m) <- c('d','e')
#' anynames(m)
#'
#' v <- c(a=1,b=2,c=3,d=4,e=5)
#' anynames(v)
#'
#' l <- list(a=1,b=2,c=3,d=4,e=5)
#' anynames(l)
NULL
