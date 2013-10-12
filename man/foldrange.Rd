\name{foldrange}
\alias{foldrange}
\title{Successively apply a function to a rolling range of a sequence}
\arguments{
  \item{x}{Any indexable data structure}

  \item{window}{The length of the sub-sequence passed to
  fn}

  \item{fn}{The function applied to the rolling range}

  \item{acc}{An object that stores the intermediate
  accumulated result}
}
\value{
  The accumulated result
}
\description{
  Apply a function to a rolling range of a sequence and the
  accumulated value of the previous function applications
}
\section{Usage}{
  foldrange(x, window, fn, acc, idx) %::% . : numeric :
  Function : . : numeric : . foldrange(x, window, fn, acc,
  0) foldrange(x, window, fn, acc=0,
  idx=length(x)-window+1)

  When ! is.null(dim(x)) foldrange(x, window, fn, acc=0)
}

\section{Details}{
  This function is the fold counterpart of maprange. It's
  primarily here for completeness purposes, as the utility
  of this function is still to be determined.
}
\examples{
\dontrun{
# Mean of rolling means
z <- sapply(1:500,
  function(x) foldrange(rnorm(50), 10, function(a,b) mean(a) + b) / 41)
}
}
\author{
  Brian Lee Yung Rowe
}
\seealso{
  \code{\link{map}} \code{\link{fold}}
  \code{\link{foldblock}}
}

