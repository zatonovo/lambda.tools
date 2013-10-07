\name{chomp}
\alias{chomp}
\title{Remove the head and tail of a data structure}
\arguments{
  \item{x}{a vector or a 2-d data structure.}

  \item{head}{the number of elements to be removed from the
  head of x}

  \item{tail}{the number of elements to be removed from the
  tail of x}
}
\value{
  A vector with the elements defined by head and tail
  removed.
}
\description{
  Remove the head and tail of a data structure
}
\section{Details}{
  This function removes the head and tail of a data
  structure. The parameters \code{head} and \code{tail}
  control how many elements are removed from the data
  structure, and they must be positive. Additionally, the
  summation of \code{head} and \code{tail} must not be
  greater than or equal to the length of the data
  structure.
}
\examples{
chomp(rnorm(10))
chomp(rnorm(10), head=2, tail=2)
chomp(matrix(rnorm(20), ncol=2))
chomp(data.frame(x=rnorm(20), y=rnorm(20)), head=5, tail=5)
}

