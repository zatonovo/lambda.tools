\name{anylength}
\alias{anylength}
\title{Get the generic length of an object}
\arguments{
  \item{data}{Any indexable data structure}
}
\value{
  For vectors and lists \code{anylength} returns
  \code{length(data)}, for matrices and data.frames
  \code{anylength} returns \code{nrow(data)}.
}
\description{
  Get the generic length of an object
}
\section{Details}{
  This function consolidates size dimensions for one and
  two dimensional data structures. The idea is that many
  operations require knowing either how long a vector is or
  how many rows are in a matrix. So rather than switching
  between length and nrow, anylength provides the
  appropriate polymorphism to return the proper value.

  When working with libraries, it is easy to forget the
  return type of a function, particularly when there are a
  lot of switches between vectors, matrices, and other data
  structures. This function along with its
  \code{\link{anynames}} counterpart provides a single
  interface for accessing this information across objects

  The core assumption is that in most cases length is
  semantically synonomous with nrow such that the number of
  columns in two-dimensional structures is less
  consequential than the number of rows. This is
  particularly true of time-based objects, such as zoo or
  xts where the number of observations is equal to the
  number of rows in the structure.

  When working with functions that are polymorphic,
  \code{lambda.r} function clauses that have guard
  conditions on the length of the input data structure x
  can use \code{anylength} instead of using \code{length}
  or \code{nrow}, which preserves polymorphism and reduces
  the number of function clauses necessary.

  \code{slice(x, expression) \%::\% a : logical : list}

  \code{slice(x, expression) \%when\% \{ length(expression)
  == length(x) \}}

  \code{slice(x, expression) \%::\% a : logical : list}

  \code{slice(x, expression) \%when\% \{ length(expression)
  == nrow(x) \}}

  \code{slice(x, expression) \%::\% a : logical : list}

  \code{slice(x, expression) \%when\% \{ length(expression)
  == anylength(x) \}}
}
\examples{
m <- matrix(c(1,2,3,4,5,6), ncol=2)
anylength(m)

v <- c(1,2,3,4,5)
anylength(v)
}

