\name{mapblock}
\alias{mapblock}
\title{Apply a function over blocks of a vector}
\arguments{
  \item{x}{Any indexable data structure}

  \item{block}{The block size used to map over}

  \item{fn}{A function applied to a block}
}
\value{
  A vector containing the result of fn appled to each block
}
\description{
  Apply a function over blocks of a data structure. This
  function implements polymorphism over lists, vectors,
  matrices and data.frames.
}
\section{Usage}{
  mapblock(x, block, fn, do.pad=FALSE)
}

\section{Details}{
  The function \code{fn} must take one required argment. If
  the block size is not a multiple of \code{anylength(x)}
  the vector returned will have \code{NA}s for indices that
  fall outside the range of the blocks.

  For two-dimensional structures, blocking occurs accross
  the columns of the data structure and the function will
  be appled to all rows for that given block of columns.
}
\examples{
x <- 1:10
# Apply mean to blocks of x - look how the length of output excluding NAs is
# length(x) / block.

x2 <- mapblock(x, 2, mean)
x2 <- mapblock(x, 2, mean, TRUE)
x5 <- mapblock(x, 5, mean)

# Apply mapblock across the columns of a matrix for two block sizes - Note
# how the function is applied to block columns.

m <- matrix(1:12, ncol=2)
mapblock(m, 1, sum)
mapblock(m, 2, sum)
}

