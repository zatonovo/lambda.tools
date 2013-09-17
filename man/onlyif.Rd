\name{onlyif}
\alias{onlyif}
\title{Conditionally apply a function to an argument}
\arguments{
  \item{fn}{}

  \item{x}{}
}
\description{
  Conditionally apply a function to an argument
}
\examples{
x <- rnorm(5)
onlyif(length(x) < 10, function(y) pad(y, 10 - length(y)), x)
}

