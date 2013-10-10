\name{pad}
\alias{pad}
\title{Pad a vector with NAs or other values.}
\arguments{
  \item{x}{Vector to be padded}

  \item{head}{The length of padded values at the head of x}

  \item{tail}{The length of padded values at the tail of x}

  \item{default}{Value to be padded (default is NA)}
}
\value{
  The input vector x will be returned with the appropriate
  value(s) padded to the head or tail, or both.
}
\description{
  This function pads a vector with \code{NA}s or other
  values.
}
\section{Details}{
  This function pads NAs or other values to the head and
  tail of a vector. This function has only been implemented
  for \code{x} being a vector. The number of elements that
  are padded to the head or tail of the vector are
  controlled by the parameters \code{head} and \code{tail},
  respectively.
}
\examples{
# Pad adds NAs to the head of x.
x <- 1:5
pad(x, 5)

# Pad adds NAs to the tail of x.
pad(x, 0, 5)

# Pad add NAs to the head and tail of x.
pad(x, 5, 5)

# Pad adds zeros to the head and tail of x.
pad(x, 5, 5, default=0)
}

