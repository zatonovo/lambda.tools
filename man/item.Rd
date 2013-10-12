\name{item}
\alias{item}
\title{Safely get an element from a vector}
\arguments{
  \item{v}{A sequence}

  \item{idx}{The index of the element to extract}
}
\value{
  Either the value of x[idx] or NA for invalid index values
}
\description{
  This function guarantees a vector of length > 1 as the
  return value of an indexing operation.
}
\section{Usage}{
  item(v, idx)
}

\section{Details}{
  Standard R indexing yields different results depending on
  the input. When either an empty vector or a NULL is
  passed to the indexing operator, an empty vector is
  returned. However, if the index is NA, the return value
  will be a vector of NAs having the same length as the
  original vector. This inconsistent behavior requires
  special handling whenever the index value is computed
  dynamically.

  This function is designed to create a consistent return
  value for a bad index value, which is defined as {NULL,
  NA, vector of length 0}. If any of these values are used
  as the index, then NA is returned instead of an empty
  vector.
}
\examples{
# Compare default behavior with item
(1:10)[NA]
item(1:10, NA)

# Negative indices are still allowed
item(1:10, -2)
}
\author{
  Brian Lee Yung Rowe
}

