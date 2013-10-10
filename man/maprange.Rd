\name{maprange}
\alias{maprange}
\title{Apply a function over a rolling range of a vector}
\arguments{
  \item{x}{A vector}

  \item{window}{Number of elements included in rolling
  range}

  \item{fn}{A function applied to the rolling range in x}
}
\value{
  a vector containing the result of fn appled to the
  rolling window.
}
\description{
  This function applies a function over a rolling range of
  a vector.
}
\section{Usage}{
  maprange(x, window, fn, do.pad=FALSE)
}

\section{Details}{
  This function is implemented using recursion and will
  throw an error if the length of \code{x} approaches
  \code{getOption('expressions') / 8.0}. This limit is due
  to R session protecting against infinite recursion via
  the expressions parameter. See \code{options}.
}
\examples{
x <- 1:10
x3 <- maprange(x, 3, sum, TRUE)

# Notice the difference in output vector when do.pad is FALSE.
x2 <- maprange(x, 2, sum)
x3 <- maprange(x, 3, sum)
}

