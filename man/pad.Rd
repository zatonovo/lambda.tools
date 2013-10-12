\name{pad}
\alias{pad}
\title{Pad a vector with some default value}
\arguments{
  \item{x}{A vector to pad}

  \item{head}{The amount to prepend}

  \item{tail}{The amount to append}

  \item{default}{The value to use for the pad}
}
\value{
  A padded sequence
}
\description{
  This function pads a vector with default values as a way
  to coerce the value to some predetermined length.
}
\section{Usage}{
  pad(x, head, tail=0, default=NA)
}

\section{Details}{
  It is common for sequence operations to return a sequence
  that is shorter than the original sequence. This
  phenomenon can be annoying when binding the output with
  the input in a regular data structure like a matrix or
  data.frame. This function prepends or appends a specified
  value to a data structure to ensure that the length of
  the data structure is compatible with another data
  structure.
}
\examples{
# A moving average results in n - window + 1 results, so pad at the
# head to get a vector of length 50
x <- abs(rnorm(50))
m <- maprange(x, 10, mean)
pad(m, 9)

# Pad at the end instead of the beginning. Note that the head must
# explicitly be set to 0
pad(m, 0, 9)

# Pad on both sides
pad(m, 4, 5)

# Use a different default value
pad(m, 9, default=0)
}
\author{
  Brian Lee Yung Rowe
}

