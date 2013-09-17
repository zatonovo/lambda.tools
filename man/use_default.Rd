\name{use_default}
\alias{use_default}
\title{Apply a default value whenever a variable is empty, NULL, or NA}
\arguments{
  \item{x}{}

  \item{default}{}
}
\description{
  Apply a default value whenever a variable is empty, NULL,
  or NA
}
\examples{
x <- sample(c(1:3,NA), 10, replace=TRUE)
map(x, function(y) use_default(y,0))
}

