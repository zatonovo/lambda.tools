\name{partition}
\alias{partition}
\title{Partition a sequence into coordinate pairs based on adjacent windows and
apply a metric function to each window}
\arguments{
  \item{x}{A sequence}

  \item{metric}{A function that maps a vector to a scalar}

  \item{radius}{The maximum window size of the adjacent
  partitions}
}
\value{
  A matrix with columns left and right where each row
  corresponds to the value of the metric function that was
  applied to adjacent windows.
}
\description{
  This function partitions a sequence into coordinate pairs
  based on adjacent windows and applies a function to each
  window.
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
\examples{
x <- 1:50
partition(x)

# Apply a function to each window and change the radius.
partition(x, metric=sum)

partition(x, metric=sum, radius=5)
}

