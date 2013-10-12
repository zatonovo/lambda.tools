\name{partition}
\alias{partition}
\title{Partition a sequence into adjacent windows and apply a metric function
to each window}
\arguments{
  \item{x}{A sequence}

  \item{metric}{A function that maps a vector to a
  real-valued scalar}

  \item{radius}{The extent of the neighborhood about the
  index point}
}
\value{
  A length(x)-1 by 2 matrix where each row represents the
  value of the metric applied to left and right
  neighborhoods about an index point.
}
\description{
  This function transforms a sequence into a rolling set of
  adjacent windows separated by a pivot point. Each window
  is passed to a metric function that yields a scalar
  value. The result is effectively a coordinate pair that
  represents the two adjacent windows.
}
\section{Usage}{
  partition(x, metric, radius) %::% . : Function : numeric
  : matrix partition(x, metric=median, radius=10)
}

\section{Details}{
  Many analysis approaches explore ways to reduce the
  dimensionality of a data set to make it easier to model.
  The opposite situation is when there is not enough
  information in the data structure as is. This
  circumstance requires a technique that can add
  dimensionality to a data structure, which is what this
  function does.

  The idea is that a sequence can yield additional
  information by comparing the neighborhoods around a given
  point. For this function, a point is an index of the
  sequence. In the 1D case, given an index k and a radius
  r, the left neighbohood is defined by [k-r+1, k] and the
  right neighborhood is defined by [k+1, k+r]. The values
  associated with each neighborhood are then applied to a
  metric function m: A^r -> R. This output becomes the
  coordinate pair (left, right).

  At the edges of the sequence the above formalism is not
  completely accurate. This is because at the edge, the
  neighborhood will be smaller than the radius, with a
  minimum size of 1. Hence the first iteration on a
  sequence will yield a left neighborhood of 1, while the
  right neighborhood will be [2, 1+r]. Whether this is
  acceptable is case-specific.

  In the future, a wrap parameter might be included that
  would emulate a loop instead of a sequence. This would be
  useful if a sequence represented a stationary time
  series.
}
\examples{
partition(1:10, mean, radius=2)
}
\author{
  Brian Lee Yung Rowe
}

