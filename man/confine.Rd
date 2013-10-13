\name{confine}
\alias{confine}
\title{Confine values to the given bounds}
\arguments{
  \item{x}{A numeric vector}

  \item{min.level}{The lower bound}

  \item{max.level}{The upper bound}
}
\value{
  A sequence with values outside of [min.level, max.level]
  clipped to those values
}
\description{
  Given a sequence this function confines the sequence
  values to within the specified bounds. This behavior is
  equivalent to clipping in digital signal processing.
}
\section{Usage}{
  confine(x, min.level, max.level) %::% numeric : numeric :
  numeric : numeric confine(x, min.level=-1, max.level=1)
}

\section{Details}{
  The confine function can be thought of a transform that
  limits the range of a sequence. Any values outside the
  range [min.level, max.level] are adjusted to be exactly
  min.level or max.level.

  Care should be taken when using this function as it is
  not always a good idea to change the value of outliers.
  Sometimes it is better to remove these values from a data
  set instead.
}
\examples{
confine(seq(-2,2, by=.1))
}
\author{
  Brian Lee Yung Rowe
}
\seealso{
  \code{\link{quantize}}
}

