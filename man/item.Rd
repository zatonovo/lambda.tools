\name{item}
\alias{item}
\title{Safely get an element from a vector}
\arguments{
  \item{v}{a vector of length n}

  \item{idx}{the index of the element to be returned by
  item}
}
\description{
  Returns NA whenever a bad index is encountered
}
\section{Details}{
  This function is designed to create a consistent return
  value for a bad index value. If a bad index value is used
  \code{item} will return NA.
}

\section{Value}{
  A scalar corresponding to the index used for the idx
  parameter.
}
\examples{
v <- rnorm(10)
item(v, 5)
item(v, 20)
}

