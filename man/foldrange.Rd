\name{foldrange}
\alias{foldrange}
\title{Successively apply a function to a sequence and the value of the
previous application over a rolling range of a vector}
\arguments{
  \item{x}{A vector}

  \item{window}{The number of elements included in the
  rolling range}

  \item{fn}{The function applied to the rolling range}

  \item{acc}{Accumulator}
}
\value{
  An object containing the accumulated result
}
\description{
  This function successively applies a function to a
  sequence and the value of the previous application over a
  rolling range of a vector.
}
\section{Usage}{
  foldrange(x, window, fn, acc=0)
}

\section{Details}{
  This function implements a linear fold operation over a
  rolling range with length defined by the window
  parameter. This function is defined for one- and two
  dimensional data structures only.  A restriction on the
  window size is that the window size must be less than the
  \code{length(x)}. The function applied to the window must
  take two arguments (i.e., a binary function).
}
\examples{
x <- 1:10
foldrange(x, 3, function(a,b) a+b)
}

