\name{maprange}
\alias{maprange}
\title{Apply a function over a rolling range of a vector}
\description{
  Apply a function over a rolling range of a vector
}
\examples{
x <- rnorm(50)
x10 <- maprange(x, mean, 10, TRUE)
x20 <- maprange(x, mean, 20, TRUE)
}

