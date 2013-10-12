\name{slice}
\alias{slice}
\title{Slice a sequence into two adjacent sub-sequences}
\arguments{
  \item{x}{An indexable data structure, typically a vector}

  \item{pivot}{The index of the pivot point in x}

  \item{inclusive}{Whether to include the pivot point in
  the second sub-sequence}

  \item{expression}{A logical expression}
}
\value{
  A list containing two sub-sequences or sub-matrices
}
\description{
  A sequence can be sliced using an explicit pivot point or
  by using a logical expression.
}
\section{Usage}{
  slice(x, pivot, inclusive=FALSE)

  slice(x, expression)
}

\section{Details}{
  This function splits a sequence into two adjacent
  sub-sequences at a pivot point or based on a logical
  expression. If a pivot point is chosen, then the
  inclusive parameter determines whether the value
  associated with the pivot should be included in both
  sub-sequences. If FALSE, then the indices of the
  sub-sequences will have the form [1, pivot], [pivot + 1,
  n], where n = |x|. If inclusive is TRUE, then the
  sub-sequences have indices of [1, pivot], [pivot, n].
  Obviously the pivot must be an element of the set of
  indices of x.

  An alternative construction is to use an expression to
  define a slice point. The first sub-sequence corresponds
  to the values where the expression evaluated to TRUE,
  while the second sequence corresponds to values when the
  expression evaluated to FALSE.

  In two dimensions only the first variant of this function
  is defined, as it cannot be guaranteed that a regular
  matrix will be generated using an arbitrary expression.
}
\examples{
# The number 4 is included in each sub-sequence
x <- 1:10
slice(x, 4, TRUE)

# With expressions, the sub-sequences are not necessarily continguous
slice(x, x \%\% 2 == 0)

# Same as above but in two dimensions
x <- matrix(1:40, ncol=4)
slice(x, 4)
}
\author{
  Brian Lee Yung Rowe
}

