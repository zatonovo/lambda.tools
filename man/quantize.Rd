\name{quantize}
\alias{quantize}
\title{Force values into bins}
\arguments{
  \item{x}{The vector whose values should be binned}

  \item{bins}{The available bins}

  \item{attractor}{The method to attract values to the
  bins}
}
\description{
  Force values into bins
}
\examples{
x <- rnorm(10, sd=4)
quantize(x)
}

