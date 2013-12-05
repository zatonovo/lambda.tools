#' Tools for functional programming in R
#'
#' This package contains a collection of functions that facilitate modeling 
#' of data using a functional programming paradigm. The idea is that 
#' using tools that are more closely connected with the 
#' idioms of mathematics will make it easier to map 
#' the mathematical model to the software model. 
#'
#' Functional programming concepts start with functions as the foundation.
#' Higher-order functions provide generalized machinery for operating
#' on data in an element-wise manner. Lambda.tools includes idiomatic
#' versions of the canonical higher-order functions, such as map and fold
#' for data structures common in R. Typically map and fold operate on
#' 1-dimensional data structures, but in R operations can also be applied
#' on 2-dimensional data structures. For example, the \code{apply}
#' function works in this manner. In lambda.tools map and fold both have
#' \code{*block} and \code{*range} counterparts. These functions operate
#' on sub-sequences or sub-matrices.
#'
#' For example, consider the following code that applies multiple
#' rotations to a collection of points.
#'
#'   ps <- t(matrix(c(0,0, 4,0, 2,4), nrow=2))
#'   rt <- matrix(c(cos(pi),-sin(pi),sin(pi),cos(pi), 
#'     cos(pi/2), -sin(pi/2), sin(pi/2), cos(pi/2)), nrow=2)
#'   mapblock(rt, 2, function(x) ps %*% x)
#' 
#' The result is a 6x2 matrix that is the union of the two rotation
#' operations.
#' 
#' Other functions included are functions to manipulate sequences,
#' such as \code{pad} a sequence to a specified length, \code{chomp}
#' the head and tail off a vector, \code{slice} a sequence into
#' two pieces based on an expression.
#'
#' Logical functions such as \code{onlyif} and \code{use_default}
#' eliminate the need for conditional blocks, which
#' can streamline code and remove the risk of poorly scoped variables.
#'
#' \tabular{ll}{
#' Package: \tab lambda.tools\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0.1\cr
#' Date: \tab 2013-10-13\cr
#' License: \tab LGPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @name lambda.tools-package
#' @aliases lambda.tools-package lambda.tools
#' @docType package
#' @exportPattern "^[^\\.]"
#' @import lambda.r
#' @author Brian Lee Yung Rowe <r@@zatonovo.com>
#' @seealso \code{\link{map}} \code{\link{fold}} \code{\link{samplerange}}
#'  \code{\link{slice}} \code{\link{onlyif}}
#'  \code{\link{quantize}} \code{\link{partition}}
#'  \code{\link{lambda.r}}
#' @keywords package attribute logic
NULL
