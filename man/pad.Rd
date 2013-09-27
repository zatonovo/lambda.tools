\name{pad}
\alias{pad}
\title{Pad a vector with NAs or other values.}
\arguments{
  \item{x}{vector to be padded}

  \item{head}{the length of padded values at the head of x}

  \item{tail}{the length of padded values at the tail of x}

  \item{default}{value to be padded (default is NA)}
}
\description{
  Pad a vector with NAs or other values.
}
\section{Value}{
  The input vector x will be returned with the appropriate
  value(s) padded to the head or tail, or both.
}
\examples{
# Pad 10 NA values to the head of x.
x <- 1:50
pad(x, 10)
# Pad 10 NAs to the tail of x.
pad(x, 0, 10)
# Pad 10 NAs to the head and tail of x.
pad(x, 10, 10)
# Pad 10 zeros to the head and tail of x.
pad(x, 10, 10, default=0)
}

