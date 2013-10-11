\name{foldblock}
\alias{foldblock}
\title{Successively apply a function to a block of a sequence and the
value of the previous application over a moving block subsequence of
a vector}
\arguments{
  \item{x}{Any Indexable data structure}

  \item{block}{The number of elements included in the
  rolling block}

  \item{fn}{The function applied to the rolling range}

  \item{acc}{Accumulator}
}
\value{
  An object containing the accumulated result
}
\description{
  This function applies a function to a moving block of a
  sequence and the previous application of the function.
}
\section{Usage}{
  foldblock(x, block, fn, acc=0)
}

\section{Details}{
  This function apples to both one-dimensional and
  two-dimensional data structures. A restriction on the
  block size is that the block size must be less than the
  length(x). The function applied to the blocks must take
  two arguments (i.e., a binary function).
}

\section{TODO}{
  This function is not working for a matrix. See github
  issue https://github.com/muxspace/lambda.tools/issues/3
}
\examples{
x <- 1:10
foldblock(x, 2, function(a,b) a+b)

# fold with a block size of 3 - Notice how the length of the output changes.
foldblock(x, 3, function(a,b) a+b)
}

