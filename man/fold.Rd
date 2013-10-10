\name{fold}
\alias{fold}
\title{Successively apply a function to a sequence and the value of the
previous application}
\arguments{
  \item{x}{Any indexable data structure}

  \item{fn}{A function applied to x}

  \item{acc}{Accumulator}
}
\value{
  An object containing the accumulated result.
}
\description{
  This function applys a function to a sequence and the
  value of the previous application, see refrences.
}
\section{Usage}{
  fold(x, fn, acc=0)
}

\section{Details}{
  This function implements a linear fold operation via
  recursion. The reduction process is accomplished by
  recursively passing x[-1] into the inner function call.
  For each call to fold, the input vector is shrinking by
  one element and the function applied to the data is
  appled to the first element of the input vector and the
  accumulator. Hence, the function applied to the blocks
  must take two arguments (i.e., a binary function).
}
\examples{
fold(rnorm(10), function(x, y) x + y)

fold(rnorm(10), function(x, y) x + y, acc=10)


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

