\name{fold}
\alias{fold}
\title{Successively apply a function to a sequence and the value of the
previous application}
\description{
  Successively apply a function to a sequence and the value
  of the previous application
}
\examples{
fold(rnorm(10), function(x,y) x+y)
}

