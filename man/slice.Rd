\name{slice}
\alias{slice}
\title{Split a sequence based on a pivot value or an expression}
\arguments{
  \item{x}{A sequence}

  \item{pivot}{The index where x will be sliced}

  \item{inclusive}{Defined as TRUE will include the value
  of x at index pivot as the first element in second half
  of the slice of x}

  \item{expression}{A logical expression used for logical
  indexing of x}
}
\value{
  A list containing the left and right vectors with respect
  to pivot.
}
\description{
  Split a sequence based on a pivot value or an expression
}
\section{Usage}{
  slice(x, pivot, inclusive=FALSE)

  slice(x, expression)
}

\section{Details}{
  This function splits a sequence based on a pivot value or
  logical expression. The inclusive parameter will either
  include or exclude the value at the pivot in the second
  element of the returned list.

  This function is defined for one- and two-dimensional
  data structures. In the two- dimensional case,
  \code{slice} will return a list where each value in the
  list is a matrix. This function is useful for splitting
  data on a value and applying a function to the smaller
  pieces.
}
\examples{
# Slice x at the 25th index and switch inclusive flag - look at second half of the
# output.
x <- 1:10
slice(x, 5, TRUE)

slice(x, 5, FALSE)

# Some examples using expressions - Notice how the ordering of the returned
# list of sequences changes.
slice(x, x < 5)

slice(x, x > 5)

# Slice a few two-dimensional objects.
A <- matrix(1:10, ncol=2)
slice(A, 3, TRUE)

df <- data.frame(col1=1:10, col2=1:10)
slice(df, 5, TRUE)

slice(df, 5, FALSE)

slice(df, df$col1 < 5)
}

