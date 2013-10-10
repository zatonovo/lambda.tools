\name{map}
\alias{map}
\title{Apply a function over each element of a vector}
\arguments{
  \item{x}{Any indexable data structure}

  \item{fn}{A function applied to elements in x}

  \item{y}{An accumulator object (vector, list, matrix,
  data.frame)}
}
\value{
  The value returned is the accumulator object \code{y}
}
\description{
  This funciton implements a map operation over different
  data types. This provides polymorphism for vectors,
  lists, matrices and data.frames.
}
\section{Usage}{
  map(x, fn, y=c())
}

\section{Details}{
  This function is implemented using recursion and will
  throw an error if the length of \code{x} approaches
  \code{getOption('expressions') / 8.0}. This limit is due
  to R session protecting against infinite recursion via
  the expressions parameter. See \code{options}.

  Recursion will decrement the length of the input object
  \code{x} and eventually the above function clause will be
  called as a result of \code{x} being empty. At that point
  \code{map} will return the accumulator \code{y}.
}
\examples{
map(rnorm(10, sd=2), quantize)

map(rnorm(10, sd=2), function(y) sum(y), y=list())

# Sum the columns of a matrix
map(matrix(rnorm(10, sd=2), ncol=2), function(y) sum(y), y=list())

# Sum the columns of a data.frame
map(matrix(rnorm(10, sd=2), ncol=2), function(y) sum(y), y=list())
}

