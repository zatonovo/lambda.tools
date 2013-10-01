\name{use_default}
\alias{use_default}
\title{Apply a default value whenever a variable is empty, NULL, or NA}
\arguments{
  \item{x}{a scalar variable}

  \item{default}{the value to replace empty, NULL, or NA}
}
\description{
  Apply a default value whenever a variable is empty, NULL,
  or NA
}
\section{Details}{
  These are equivlaent operations,

  \code{x <- sample(c(1:3, NA), 10, replace=TRUE)}
  \code{x[is.na(x)] <- 0} \code{map(x, function(y)
  use_default(y,0))}

  The value in using \code{use_default} instead of element
  replacement by a set operation is that the functional
  definition will help mathematical provability of the
  program and facilitate translation between the
  mathematical model and code.
}
\examples{
# Clean data.
x <- sample(c(1:3,NA), 10, replace=TRUE)
map(x, function(y) use_default(y, 0))
}

