\name{anynames}
\alias{anynames}
\alias{anynames<-}
\title{Get the names of a data structure. This attempts to
create some polymorphism around lists, vectors, and data.frames}
\arguments{
  \item{data}{Any indexable data structure}
}
\value{
  Returns the names for a data structure.
}
\description{
  This function gets the useful names of a data structure.
  This attempts to create some polymorphism around lists,
  vectors, and data.frames.
}
\section{Usage}{
  anynames(data)
}

\section{Details}{
  Depending on the type of structure utilized in code, one
  needs to call either names or \code{colnames} to get
  information related to the data sets within the
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
\examples{
m <- matrix(c(1,2,3,4,5,6), ncol=2)
anynames(m) <- c('d','e')
anynames(m)

v <- c(a=1,b=2,c=3,d=4,e=5)
anynames(v)

l <- list(a=1,b=2,c=3,d=4,e=5)
anynames(l)

df <- data.frame(a=1:10, b=1:10,c=1:10,d=1:10,e=1:10)
anynames(df)
}
\author{
  Brian Lee Yung Rowe
}
\keyword{attribute}

