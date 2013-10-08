\name{fold}
\alias{fold}
\title{Successively apply a function to a sequence and the value of the
previous application}
\arguments{
  \item{x}{Any indexable data structure}

  \item{fn}{a function applied to x}

  \item{acc}{accumulator}
}
\value{
  An object containing the accumulated result.
}
\description{
  Successively apply a function to a sequence and the value
  of the previous application
}
\section{Details}{
  This function implements a linear fold operation. The
  function applied to the blocks must take two arguments
  (i.e., a binary function).
}
\examples{
fold(rnorm(10), function(x, y) x + y)

fold(rnorm(10), function(x, y) x + y, acc=10)

# Fold over a list element.
x <- list(1:10)
fold(x[[1]], function(x, y) x + y)

# Fold across the columns of a matrix.
x <- matrix(1:10, ncol=2)
fold(x, function(x, y) x + y)

# Fold accross the rows of a data.frame.
x <- data.frame(x1=1:10, x2=1:10)
fold(x, function(x, y) x + y)
}
\references{
  Haskell Wiki, http://www.haskell.org/haskellwiki/Fold

  Brian Lee Yung Rowe, Modeling Data with Functional
  Programming in R.
}

