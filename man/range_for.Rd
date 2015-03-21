\name{range_for}
\alias{range_for}
\title{Find contiguous ranges of a given value within a sequence}
\arguments{
  \item{target}{A value to find in x}

  \item{x}{A vector}
}
\value{
  A data.frame where each row specifies the end points of a
  contiguous range that contains the target value
}
\description{
  Identify the index ranges for a given value in a sequence
  and return the minimum and maximum values of the ranges.
}
\section{Usage}{
  range_for(target, x)
}
\examples{
# Find all contiguous ranges containing 2
x <- sample(c(1,2,2,2,3,4), 20, replace=TRUE)
range_for(2,x)
}
\author{
  Brian Lee Yung Rowe
}

