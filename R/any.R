#' Get the generic length of an object 
#' 
#' This function gets the generic length of an object.
#'
#' @section Usage: 
#' anylength(data)
#'
#' @name anylength 
#' @param data Any indexable data structure
#'
#' @section Details:
#' This function consolidates size dimensions for one and two dimensional data
#' structures. The idea is that many operations require knowing either how long
#' a vector is or how many rows are in a matrix. So rather than switching
#' between \code{length} and \code{nrow}, \code{anylength} provides the appropriate 
#' polymorphism to return the proper value. 
#'
#' When working with libraries, it is easy to forget the return type of a
#' function, particularly when there are a lot of switches between vectors,
#' matrices, and other data structures. This function along with its
#' \code{\link{anynames}} counterpart provides a single interface for accessing
#' this information across objects
#' 
#' The core assumption is that in most cases length is semantically synonomous
#' with \code{nrow} such that the number of columns in two-dimensional structures is
#' less consequential than the number of rows. This is particularly true of
#' time-based objects, such as zoo or xts where the number of observations is
#' equal to the number of rows in the structure.
#'
#' When working with functions that are polymorphic, \code{lambda.r} function
#' clauses that have guard conditions on the length of the input data structure x
#' can use \code{anylength} instead of using \code{length} or \code{nrow},
#' which preserves polymorphism and reduces the number of function clauses necessary. 
#' For example, 
#'
#' \code{slice(x, expression) \%::\% a : logical : list}
#'
#' \code{slice(x, expression) \%when\% \{ length(expression) == length(x) \}}
#'
#'
#' \code{slice(x, expression) \%::\% a : logical : list}
#'
#' \code{slice(x, expression) \%when\% \{ length(expression) == nrow(x) \}}
#'
#'
#' \code{slice(x, expression) \%::\% a : logical : list}
#'
#' \code{slice(x, expression) \%when\% \{ length(expression) == anylength(x) \}}
#'
#' @return For vectors and lists \code{anylength} returns \code{length(data)}, for 
#' matrices and data.frames \code{anylength} returns \code{nrow(data)}.
#'
#' @examples
#' # anylength can be used in place of nrows or length to get the generic length of
#' # an object.
#'
#' m <- matrix(c(1,2,3,4,5,6), ncol=2)
#' anylength(m)
#'
#' v <- c(1,2,3,4,5)
#' anylength(v)
#
anylength(data) %when% { ! is.null(nrow(data)) } %as% nrow(data)
anylength(data) %as% length(data)

#' Get the useful names of a data structure. This attempts to 
#' create some polymorphism around lists, vectors, and data.frames
#' 
#' This function gets the useful names of a data structure. This attempts to 
#' create some polymorphism around lists, vectors, and data.frames.
#'
#' @section Usage:
#' anynames(data)
#'
#' @name anynames
#' @aliases anynames<-
#'
#' @param data Any indexable data structure
#'
#' @section Details:
#' Depending on the type of structure utilized in code, one needs to call either
#' names or \code{colnames} to get information related to the data sets within the 
#' structure. The use of two separate functions can cause errors and slows
#' development time as data structures passed from intermediate functions may
#' change over time, resulting in a broken interface.
#'
#' By providing a thin layer over underlying accessors, this function attempts to
#' expedite development and add a bit of polymorphism to the semantics of names.
#' The explicit assumption is that data sets in two dimensional structures are
#' organized by column, as this is compatible with time-series objects such as
#' zoo and xts. 
#'
#' @return Returns the names for a data structure.
#' @examples
#'
#' # These examples illustrate anynames provides a clean interface for 
#' # different named data structures. The function anynames can be used in place of
#' # names or colnames.
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
#' 
#' df <- data.frame(a=1:10, b=1:10,c=1:10,d=1:10,e=1:10)
#' anynames(df)
anynames(data) %when% { ! is.null(names(data)) } %as% names(data)
anynames(data) %when% { ! is.null(colnames(data)) } %as% colnames(data)
anynames(data) %as% NULL

"anynames<-" <- function(data, value)
{
  if (is.null(names(data))) colnames(data) <- value
  else names(data) <- value
  invisible(data)
}

#' Lists out the types of a data.frame
#'
#' This function lists the types of the columns in a data.frame. 
#'
#' @section Usage:
#' anytypes(data, fn=class) 
#' 
#' @name anytypes
#' @param data A data.frame
#' @param fun The function used to get the types. Defaults to class, although
#'    type or mode, etc. could be used
#'
#' @section Details:
#' Depending on the type of structure utilized in code, one needs to call either
#' \code{names} or \code{colnames} to get information related to the data sets
#' within the structure. The use of two separate functions can cause errors and slows
#' development time as data structures passed from intermediate functions may
#' change over time, resulting in a broken interface.
#'
#' By providing a thin layer over underlying accessors, this function attempts to
#' expedite development and add a bit of polymorphism to the semantics of names.
#' The explicit assumption is that data sets in two dimensional structures are
#' organized by column, as this is compatible with time-series objects such as
#' zoo and xts.
#'
#' @section TODO:
#' Implement \code{anytypes} for a list. See github issue
#' https://github.com/muxspace/lambda.tools/issues/2
#'
#' @return Returns a vector containing the types of the columns of a data structure.
#'
#' @examples
#' # Get the types of the columns of a data.frame
#' d <- data.frame(ints=c(1,2,3), chars=c('a','b','c'), nums=c(.1,.2,.3))
#' anytypes(d)
anytypes(data, fun=class) %when% {
  is.null(dim(data))
} %as% fun(data)

anytypes(data, fun=class) %as% {
  ts <- apply(matrix(anynames(data), ncol=1), 1, function(x) fun(data[,x]))
  names(ts) <- anynames(data)
  return(ts)
}

# Check whether data is bad or empty
is.empty(x) %::% a : logical
is.empty(x) %as% { length(x) < 1 }

is.bad(x) %::% a : logical
is.bad(x) %when% { is.null(x) } %as% TRUE
is.bad(x) %when% { is.empty(x) } %as% TRUE

is.bad(x) %::% list : list
is.bad(x) %as% { lapply(x, is.bad) }

is.bad(x) %::% data.frame : matrix
is.bad(x) %as% { sapply(x, is.bad) }

is.bad(x) %::% matrix : matrix
is.bad(x) %as% { apply(x,1, is.bad) }

is.bad(x) %::% a : logical
is.bad(x) %as% { is.na(x) }
