\name{partition}
\alias{partition}
\title{Partition a sequence into coordinate pairs based on adjacent windows}
\arguments{
  \item{x}{a sequence}

  \item{metric}{a function that maps a vector to a scalar}

  \item{radius}{the maximum window size of the adjacent
  partitions}
}
\description{
  Partition a sequence into coordinate pairs based on
  adjacent windows
}
\section{Details}{
  If the metric function does not map a vector to a scalar,
  \code{partition} will throw an error. The metric function
  will be applied to each partition and the value of the
  function will be returned for each panel in the resulting
  matrix (left or right panel).

  The radius parameter controls the size of the partitions.
  The partition size of the windows remains constant until
  one of the windows approach the head or tail of the
  sequence. In that case the respective window will have a
  length less than the radius parameter.
}

\section{Value}{
  A matrix with columns left and right where each row
  corresponds to the value of the metric function that was
  applied to adjacent windows.
}
\examples{
x <- 1:50
partition(x)
partition(x, metric=sum)
partition(x, metric=sum, radius=5)
}

