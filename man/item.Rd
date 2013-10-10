\name{item}
\alias{item}
\title{Safely get an element from a vector}
\arguments{
  \item{v}{A vector of length n}

  \item{idx}{The index of the element to be returned by
  item}
}
\value{
  A scalar corresponding to the index used for the idx
  parameter
}
\description{
  This function returns an element from a vector or NA
  whenever a bad index is encountered.
}
\section{Usage}{
  item(v, idx)
}

\section{Details}{
  This function is designed to create a consistent return
  value for a bad index value. If a bad index value is used
  \code{item} will return NA.
}
\examples{
# Get an element from v with a good and bad index - notice the return value
# for the second funciton call.
v <- rnorm(10)
item(v, 5)

item(v, 20)
}

