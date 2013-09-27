\name{quantize}
\alias{quantize}
\title{Force values into bins}
\arguments{
  \item{x}{The vector whose values should be binned}

  \item{bins}{The available bins}

  \item{metric}{The method to attract values to the bins}
}
\description{
  Force values into bins
}
\examples{
x <- seq(-2, 2, by=.1)
quantize(x)
# quantize x using a Euclidian distance metric.
quantize(x, metric=function(a, b) sqrt((a - b)^2))
# quantize with custom bins.
quantize(x, bins=c(-2, -1, 0, 1, 2), metric=function(a, b) sqrt((a - b)^2))
}

