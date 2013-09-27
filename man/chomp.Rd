\name{chomp}
\alias{chomp}
\title{Remove the head and tail of a data structure}
\arguments{
  \item{x}{}

  \item{head}{}

  \item{tail}{}
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

