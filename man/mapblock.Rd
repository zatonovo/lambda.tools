\name{mapblock}
\alias{mapblock}
\title{Apply a function over blocks of a vector}
\arguments{
  \item{x}{Any indexable data structure}

  \item{block}{The block size used to map over}

  \item{fn}{A function applied to a block}
}
\value{
  A vector containing the result of fn applied to each
  block
}
\description{
  This form of map operates on non-overlapping adjacent
  blocks of a data structure.
}
\section{Usage}{
  mapblock(x, window, fn, do.pad=FALSE)
}

\section{Details}{
  This function is useful primarily in the two-dimensional
  form. The use case is when a number of rotation matrices
  should be applied to a set of points. By collecting all
  the rotation matrices into a larger matrix, it is easy to
  produce a map process along the sub-matrices in a way
  that doesn't require managing indices.

  The 1D version is provided for completeness and is
  equivalent to a 2D map, except on the edge cases.
}
\examples{
# The 1D version is equivalent to a 2D map
x <- 1:24
mapblock(x, 4, sum) == map(matrix(x,nrow=4), sum)

# Sum sub-sequences of a sequence. Note that the last value will have
# fewer elements
mapblock(1:10, 3, sum)

# Pad at the head of the sequence to yield an integer multiple of window
mapblock(1:10, 3, function(x) sum(x, na.rm=TRUE), do.pad=TRUE)
}
\author{
  Brian Lee Yung Rowe
}

