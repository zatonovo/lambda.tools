\name{is.scalar}
\alias{is.scalar}
\title{Check if an object is a scalar}
\arguments{
  \item{x}{An object}
}
\value{
  A logical value that indicates if the input is of length
  one
}
\description{
  This function checks if an object is a scalar.
}
\section{Usage}{
  is.scalar(x)
}

\section{Details}{
  This function checks to determine if an object \code{x}
  is a scalar, i.e. the length of the object is equal to
  one.
}
\examples{
is.scalar(10)

is.scalar(1:10)
}

