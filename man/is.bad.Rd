\name{is.bad}
\alias{is.bad}
\alias{is.empty}
\title{ Check whether data is bad or empty }
\description{
These functions quickly test whether data within an object has bad values or
if the object is defined (i.e. not null) but has no data.
}
\usage{
is.bad(...)

is.empty(...)
}
\arguments{
  \item{\dots}{ Abstract function controlled by lambda.r }
}
\details{
  is.empty(x) \%::\% a : logical

  is.bad(x) \%::\% list : logical

  is.bad(x) \%::\% data.frame : matrix

  is.bad(x) \%::\% matrix : matrix

  is.bad(x) \%::\% a : logical


  x - The data to test

  Depending on the type of an object, knowing whether an object contains a
  valid value or not is different. These functions unify the interfaces across
  different data types quickly indicating whether an object contains bad
  values and also whether an object has a value set. 

  For example, a data.frame may be initialized with no data. This results in
  an object that is non-null but also unusable. Instead of checking whether
  something is both non-null and has positive length, just check is.bad().

  If you know that an object is non-null, then you can call is.empty() which
  is a shortcut for checking the length of an object.
}
\value{
  Logical values that indicate whether the test was successful or not. For
  matrices and data.frames, a matrix of logical values will be returned.
}
\author{ Brian Lee Yung Rowe }
\examples{
a <- data.frame(a=NULL, b=NULL)
is.bad(a)

b <- list(a=1:3, b=NULL, c=NA, d='foo')
is.bad(b)

c <- list()
is.empty(c)

}
\keyword{ attribute  }
