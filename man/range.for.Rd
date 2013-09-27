\name{range.for}
\alias{range.for}
\title{Get the range of a value in a series}
\arguments{
  \item{value}{a value in series (can be unique or
  repeating in series)}

  \item{series}{a series of values}
}
\description{
  Get the range of a value in a series
}
\section{Details}{
  This function is only defined for a one-dimensional
  vector and will throw an error if a two-dimensional input
  is used. The function \code{segment} is used to shift the
  series and scan the rows of the \code{data.frame} for
  matching values. If two values are the same on a specific
  row then adjacent data can be detected.
}

\section{Value}{
  A \code{data.frame} where each row contains the start and
  end index for the range of the value of interest in the
  series.
}

\section{TODO}{
  Implement a \code{range.for} clause that can find the
  range of a subsequence in a sequence. For example:

  \code{sequence <- c(seq(1, 10), c(10, 10), seq(11, 20))}

  \code{range.for(c(10, 10, 10), sequence)}

  The above call to \code{range.for} should return the
  range for the triplet \code{c(10, 10, 10)}.
}

\section{See Also}{
  \code{segment}
}
\examples{
series <- c(seq(1, 25), c(25, 25), seq(26, 50))
range.for(25, series)
}

