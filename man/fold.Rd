\name{fold}
\alias{fold}
\title{Successively apply a function to a sequence and the value of the
previous application}
\arguments{
  \item{x}{a vector, list, matrix or data.frame}

  \item{fn}{a function applied to x}

  \item{acc}{accumulator}
}
\description{
  Successively apply a function to a sequence and the value
  of the previous application
}
\examples{
fold(rnorm(10), function(x, y) x + y)
fold(rnorm(10), function(x, y) x + y, acc=10)
# Fold over a list element.
x <- list(1:10)
fold(x[[1]], function(x, y) x + y)
# Fold across the rows of a matrix.
x <- matrix(1:10, ncol=2)
fold(x, function(x, y) x + y)
# Fold accross the rows of a data.frame.
x <- data.frame(x1=1:10, x2=1:10)
fold(x, function(x, y) x + y)
}

