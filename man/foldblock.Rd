\name{foldblock}
\alias{foldblock}
\title{Successively apply a function to adjacent blocks of a sequence}
\arguments{
  \item{x}{Any indexable data structure}

  \item{window}{The number of elements in each
  sub-sequence}

  \item{fn}{The function applied to the sub-sequence}

  \item{acc}{The intermediate accumulated value}
}
\value{
  The accumulated value
}
\description{
  Apply a function to non-overlapping sub-sequences and the
  accumulated value of the function application
}
\section{Usage}{
  foldblock(x, window, fn, acc=0)
}

\section{Details}{
  This function is the fold counterpart of mapblock. It's
  primarily here for completeness purposes, as the utility
  of this function is still to be determined.
}
\examples{
\dontrun{
# Mean of means
z <- sapply(1:200,
  function(x) foldblock(rnorm(500), 10, function(a,b) mean(a) + b) / 50)
}
}
\author{
  Brian Lee Yung Rowe
}
\seealso{
  \code{\link{map}} \code{\link{fold}}
  \code{\link{foldrange}}
}

