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
# Slice a one-dimensional sequence.
x <- 1:50
slice(x, 25, TRUE)

slice(x, 25, FALSE)

slice(x, x < 25)

slice(x, x > 25)

slice(x, x > 10 & x < 25)

slice(x,  x > 10 & x < 25)

# Slice a matrix.
A <- matrix(1:10, ncol=2)
slice(A, 3, TRUE)

slice(A, 3, FALSE)

slice(A,  A[,1] > 5 & A[,1] < 7)

# Slice a data frame.
x <- 1:50
df <- data.frame(col1=x, col2=x)
slice(df, 25, TRUE)

slice(df, 25, FALSE)

slice(df, df$col2 > 30)

slice(df, df$col1 > 10 & df$col1 < 25)
}

