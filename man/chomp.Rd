\name{chomp}
\alias{chomp}
\title{Remove the head and tail of a data structure}
\arguments{
  \item{x}{a vector or a 2-d data structure.}

  \item{head}{the number of elements to be removed from the
  head of x}

  \item{tail}{the number of elements to be removed from the
  tail of x}
}
\description{
  Remove the head and tail of a data structure
}
\examples{
chomp(rnorm(10))
chomp(rnorm(10), head=2, tail=2)
chomp(matrix(rnorm(20), ncol=2))
chomp(data.frame(x=rnorm(20), y=rnorm(20)), head=5, tail=5)
}

