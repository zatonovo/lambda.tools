\name{fold}
\alias{fold}
\title{Successively apply a function to the elements of a sequence}
\arguments{
  \item{x}{Any indexable data structure}

  \item{fn}{A binary operator}

  \item{acc}{Accumulator}
}
\value{
  An object containing the accumulated result.
}
\description{
  Apply a function to each element of a sequence and the
  accumulated value of the previous function applications
}
\section{Usage}{
  fold(x, fn, acc) \%::\% . : Function : . : .

  fold(x, fn, acc=0)
}

\section{Details}{
  The fold operation is a generalization of the summation
  and product operators in mathematics. The idea is that
  the elements of a sequence can have a function applied to
  them and then can be aggregated in some arbitrary way. In
  terms of the summation operator, the general structure is
  sum f(x_i). This means that the function f is applied to
  each element of x and then added to some intermediate
  accumulator. This is equivalent to a function f' : A x B
  -> B where the single function is responsible for both
  applying f and also aggregating the accumulated value.

  A 2D fold is similar to a 2D map in the sense that the
  function operates on the columns of x. This indicates
  that fn takes a vector and not a scalar as the first
  argument. If fn is vectorized, then the behavior of fold
  will be equivalent to a 2D map over the rows!
}
\examples{
x <- 1:10

# This is equivalent to the summation operator
sum(x) == fold(x, function(a,b) a+b)
sum(x^2) == fold(x, function(a,b) a^2 + b)

# This is equivalent to the product operator
prod(x) == fold(x, function(a,b) a*b, 1)

# Note the equivalence with map
x <- matrix(1:24, ncol=4)
map(t(x), function(a) sum(a)) == fold(x, function(a,b) a + b)
}
\author{
  Brian Lee Yung Rowe
}
\references{
  Haskell Wiki, http://www.haskell.org/haskellwiki/Fold

  Brian Lee Yung Rowe, Modeling Data With Functional
  Programming In R.
}
\seealso{
  \code{\link{map}} \code{\link{foldrange}}
  \code{\link{foldblock}}
}

