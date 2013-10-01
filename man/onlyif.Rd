\name{onlyif}
\alias{onlyif}
\title{Conditionally apply a function to an argument}
\arguments{
  \item{condition}{logical statement used to conditionally
  apply fn to x}

  \item{fn}{a function to apply to x}

  \item{x}{an object}
}
\description{
  Conditionally apply a function to an argument
}
\section{Details}{
  This function can be used to apply a function to a vector
  containing elements that lie outside the valid domain of
  \code{fn}. Th function \code{onlyif} differs from
  \code{ifelse} in the sense that it is not vectorized and
  a closure can be used. For example,

  \code{ifelse(length(x) < 10, function(y) fold(x,
  function(x, y) x+y), x)}.

  will fail due to the closure around \code{fold}.  If the
  argument \code{fn}, is not a function \code{onlyif} will
  throw an error.
}
\examples{
x <- rnorm(5)
onlyif(length(x) < 10, function(y) pad(y, 10 - length(y)), x)
onlyif(length(x) < 10, function(y) fold(x, function(x, y) x+y), x)
}

