\name{confine}
\alias{confine}
\title{Confine values to the given bounds}
\arguments{
  \item{x}{A vector}

  \item{min.level}{Lower bound on values to be confined}

  \item{max.level}{Upper bound on values to be confined}
}
\value{
  A vector of points that lie within the bounds defined by
  min.level and max.level.
}
\description{
  This function confines the values of a vector to the
  given bounds.
}
\section{Usage}{
  confine(x, min.level=-1, max.level=1)
}

\section{Details}{
  This function confines a set of values within a lower and
  upper bound. The function defintion is only written for x
  being a vector and is not vectorized to handle two
  dimensional data structures.
}
\examples{
# Confine the values in x to lie betwween [-1, 1].
x <- 1:100
y <- rnorm(100, sd=4)
confine(y)

# Note the effect of min.level and max.level.
plot(x, y)
points(x, confine(y), col='red')
points(x, confine(y, min.level=-2, max.level=2), col='blue')
points(x, confine(y, min.level=-3, max.level=3), col='green')
}

