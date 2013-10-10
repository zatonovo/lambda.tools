\name{quantize}
\alias{quantize}
\title{Force values into bins}
\arguments{
  \item{x}{The vector whose values should be binned}

  \item{bins}{The available bins}

  \item{metric}{The method to attract values to the bins}
}
\value{
  a vector containing quantized data.
}
\description{
  This function quantizes data based on a metric function.
  The effect is that values in x will be forced into the
  set of given bins.
}
\section{Usage}{
  quantize(x, bins=c(-1,0,1), metric=function(a,b)
  abs(a-b))
}

\section{Details}{
  This function forces values into a set of bins using a
  distance metric. This process is referred to as
  quantizing and is useful in signal processing or
  classification. The metric used can be a function such as
  euclidian distance or absolute distance, for example.

  For each value in \code{x}, this \code{quantize}
  evaluates the metric function with the value of \code{x}
  and all defined bins. The smallest metric value is
  selected and used to force the value of x to the nearest
  bin. The metric function should be binary, otherwise a
  comparison between the value of x and the bin can not be
  made.
}
\examples{
x <- seq(-2, 2, by=.1)
quantize(x)

# quantize x using a Euclidian distance metric.
quantize(x, metric=function(a, b) sqrt((a - b)^2))

# Notice the difference in the return vector when compared to the above examples.
quantize(x, bins=c(-2, -1, 0, 1, 2), metric=function(a, b) sqrt((a - b)^2))
}

