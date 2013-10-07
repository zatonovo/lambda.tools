\name{segment}
\alias{segment}
\title{Similar to partion where radius=2 and there is no metric}
\arguments{
  \item{x}{a sequence}

  \item{padded}{logical to control if NAs will be padded to
  segments}
}
\description{
  Similar to partion where radius=2 and there is no metric
}
\section{Details}{
  This function is only defined for a one-dimensional
  vector and will throw an error if a two-dimensional data
  structure is used for input. The \code{segment} function
  will shift a sequence x by one element and return a
  two-column \code{data.frame} containing the shifted
  sequences.

  Segmenting the sequences in this manner is useful for
  finding the boundaries of adjacent values.
}

\section{Value}{
  Returns a two-column \code{data.frame} containing the
  shifted sequence.
}

\section{See Also}{
  \code{range.for}
}
\examples{
x <- 1:50
segment(x)
segment(x, TRUE)
}

