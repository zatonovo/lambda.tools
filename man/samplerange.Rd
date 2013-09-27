\name{samplerange}
\alias{samplerange}
\title{Get a sample as a subsequence of a larger set}
\arguments{
  \item{x}{a 1-d or 2-d data structure}

  \item{size}{sample size}

  \item{window}{the length of the output vectors}

  \item{\dots}{optional arguments for the sample.int
  function}
}
\value{
  A matrix containing the samples from x stored column-wise
}
\description{
  Get a sample as a subsequence of a larger set
}
\section{Details}{
  If the window parameter is defined to be greater than the
  length of the input vector, this function will throw an
  error. This function can be applied to both one- and
  two-dimensional data structures.

  A fixed interger set of sample size \code{size} is
  sampled randomly based on the the length of the input
  vector \code{x} and the window parameter. This fixed
  integer set is used to index the values in the vector x.
}

\section{Value}{
  Returns a matrix with the number of rows equal to
  \code{size} and the number of columns equal to
  \code{window}.
}
\examples{
# Sample a range in a vector.
x <- rnorm(10)
samplerange(x, 5, 2)
samplerange(x, 5, 3)
# Sample a range in a matrix.
x <- matrix(rnorm(10), ncol=2)
samplerange(x, 4, 2)
}

