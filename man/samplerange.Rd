\name{samplerange}
\alias{samplerange}
\title{Get a sample as a subsequence of a larger set}
\arguments{
  \item{x}{A one-dimensional or two-dimensional data
  structure}

  \item{size}{Sample size}

  \item{window}{The length of the output vectors}

  \item{\dots}{Optional arguments for the sample.int
  function}
}
\value{
  A matrix containing the samples from x stored
  column-wise.
}
\description{
  Get a sample as a subsequence of a larger set
}
\section{Details}{
  If the window parameter is defined to be greater than the
  length of the input vector, this function will throw an
  error. This function can be applied to both one- and
  two-dimensional data structures.

  A fixed integer set of a sample size is sampled randomly
  based on the the length of the input vector \code{x} and
  the window parameter. This fixed integer set is used to
  index the values in the vector x.
}
\examples{
# Sample a range in a vector.
x <- 1:10
samplerange(x, 5, 2)

samplerange(x, 5, 3)

# Sample a range in a matrix.
x <- matrix(rnorm(10), ncol=2)
samplerange(x, 4, 2)
}

