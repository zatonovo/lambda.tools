\name{onlyif}
\alias{onlyif}
\title{Conditionally apply a function to an argument}
\arguments{
  \item{condition}{Logical statement used to conditionally
  apply fn to x}

  \item{fn}{A function to apply to x}

  \item{x}{An object}
}
\value{
  Either \code{fn(x)} if \code{condition} is true,
  otherwise \code{x}.
}
\description{
  This function conditionally applies a function to an
  argument given a logical condition.
}
\section{Usage}{
  onlyif(condition, fn, x)
}

\section{Details}{
  This function can be used to apply a function to a vector
  containing elements that lie outside the valid domain of
  \code{fn}. The function \code{onlyif} differs from
  \code{ifelse} in the sense that it is not vectorized and
  a closure can be used. For example,

  \code{ifelse(length(x) < 10, function(y) fold(x,
  function(x,y) x+y), x)}

  will fail due to the closure around \code{fold}. The
  alternative would be

  \code{onlyif(length(x) < 10, function(y) fold(x,
  function(x,y) x+y), x)}.
}
\examples{
x <- 1:5
onlyif(length(x) < 10, function(y) pad(y, 10 - length(y)), x)

# This returns x
x <- 1:20
onlyif(length(x) < 10, function(y) pad(y, 10 - length(y)), x)
}

