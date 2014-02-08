\name{onlyif}
\alias{onlyif}
\title{Conditionally apply a function to an argument}
\arguments{
  \item{condition}{Logical statement used to conditionally
  apply fn to x}

  \item{fn}{A function to apply to x}

  \item{expr}{An expression}

  \item{x}{An object}
}
\value{
  Either \code{expr} if \code{condition} is true, otherwise
  \code{x}.
}
\description{
  This function conditionally applies a function to an
  argument given a logical condition.
}
\note{
  The interface for this function is experimental. I'm
  looking for a way to preserve unevaluated expressions.
  Until then, I don't recommend using the function.
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

  \code{ifelse(length(x) < 10, pad(x, 10 - length(x)), x)}

  yields the wrong result due to the length of the first
  argument. The \code{onlyif} function is designed for
  these situations.

  \code{onlyif(length(x) < 10, function(x) pad(x, 10 -
  length(x)), x)}.

  Note that a closure is only required if an expression
  cannot be evaluated under both a TRUE or FALSE scenario.

  The alternative would be to use a conditional block,
  which can result in improperly scoped code if one is
  careless.
}
\examples{
x <- 1:5
onlyif(length(x) < 10, pad(x, 10 - length(x)), x)
onlyif(length(x) < 10, function(x) pad(x, 10 - length(x)), x)

# This returns x
x <- 1:20
onlyif(length(x) < 10, function(x) pad(x, 10 - length(x)), x)
}
\seealso{
  \code{\link{use_default}}
}

