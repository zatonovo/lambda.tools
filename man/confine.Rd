\name{confine}
\alias{confine}
\title{Confine values to the given bounds}
\description{
  Confine values to the given bounds
}
\examples{
x <- 1:100
y <- rnorm(100, sd=4)
confine(y)
# Illustration of confine(y) behavior.
plot(x, y)
points(x, confine(y), col='red')
points(x, confine(y, min.level=-2, max.level=2), col='blue')
points(x, confine(y, min.level=-3, max.level=3), col='green')
}

