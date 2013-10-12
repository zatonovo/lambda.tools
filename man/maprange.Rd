\name{maprange}
\alias{maprange}
\title{Apply a function over a rolling range of a data structure}
\arguments{
  \item{x}{Any indexable data structure}

  \item{window}{The length of the sub-sequence to pass to
  fn}

  \item{fn}{A function applied to a rolling range of x}

  \item{do.pad}{Whether to pad the output to be the same
  length as the input}
}
\value{
  In the 1D case, a vector of length(x) - window + 1
  (unless padded) will be returned. Otherwise a matrix with
  dimension length(x) - window + 1 by ncol(x) will be
  returned.
}
\description{
  Either applies a function over a rolling range of a
  sequence or multiple sequences bound as a matrix or
  data.frame.
}
\section{Usage}{
  maprange(x, window, fn, do.pad=FALSE)
}

\section{Details}{
  This function is intended to work primarily with time
  series-like objects where the same statistic is computed
  over a rolling window of the time series. In other
  packages this operation is referred to as rollapply (e.g.
  zoo). This version has two significant differences from
  other implementations: 1) it is purely functional, and
  therefore easy to reason about; 2) it has consistent
  semantics with the family of map functions.

  Comparing the code for zoo:::rollapply.zoo, which is
  close to 100 lines, versus the 3 lines separated into 2
  function clauses clearly demonstrates the conciseness
  inherent in functional programming. Mathematics is known
  for being very compact and powerful. When utilized
  appropriately, functional programs share this same
  property.
}
\examples{
# Compute a 5-period moving average over a vector
maprange(rnorm(20), 5, mean, do.pad=TRUE)

# Same as above, but do it for 4 time series
maprange(matrix(rnorm(80),ncol=4), 5, mean, do.pad=TRUE)
}
\author{
  Brian Lee Yung Rowe
}
\seealso{
  \code{\link{map}} \code{\link{mapblock}}
}

