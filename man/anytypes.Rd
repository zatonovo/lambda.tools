\name{anytypes}
\alias{anytypes}
\title{Lists out the types of a data.frame}
\arguments{
  \item{data}{A data.frame}

  \item{fun}{The function used to get the types. Defaults
  to class, although type or mode, etc. could be used}
}
\value{
  Returns a vector containing the types of the columns of a
  data structure.
}
\description{
  This function lists the types of the columns in a
  data.frame.
}
\section{Usage}{
  anytypes(data, fn=class)
}

\section{Details}{
  Depending on the type of structure utilized in code, one
  needs to call either \code{names} or \code{colnames} to
  get information related to the data sets within the
  structure. The use of two separate functions can cause
  errors and slows development time as data structures
  passed from intermediate functions may change over time,
  resulting in a broken interface.

  By providing a thin layer over underlying accessors, this
  function attempts to expedite development and add a bit
  of polymorphism to the semantics of names. The explicit
  assumption is that data sets in two dimensional
  structures are organized by column, as this is compatible
  with time-series objects such as zoo and xts.
}

\section{TODO}{
  Implement \code{anytypes} for a list. See github issue
  https://github.com/muxspace/lambda.tools/issues/2
}
\examples{
# Get the types of the columns of a data.frame
d <- data.frame(ints=c(1,2,3), chars=c('a','b','c'), nums=c(.1,.2,.3))
anytypes(d)
}

