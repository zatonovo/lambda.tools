% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/any.R
\name{anytypes}
\alias{anytypes}
\title{Show the types of a list or data.frame}
\arguments{
\item{data}{A data.frame}

\item{fn}{The function used to get the types. Defaults to class, although
type or mode, etc. could be used}
}
\value{
A vector containing the types of the columns of a data structure
}
\description{
This function shows the types of the columns in a data.frame or the 
elements of a list.
}
\section{Usage}{

anytypes(data, fn) %::% list : Function : character
anytypes(data, fn=class) 

anytypes(data, fn) %::% data.frame : Function : character
anytypes(data, fn=class)
}

\section{Details}{

This is a convenience function to see the types associated with the
elements of a list or the columns of a data.frame.
}

\examples{
x <- data.frame(ints=1:3, chars=c('a','b','c'), nums=c(.1,.2,.3))
anytypes(x)

x <- list(ints=1:4, chars=c('a','b','c'), nums=c(.1,.2,.3))
anytypes(x)
}
\author{
Brian Lee Yung Rowe
}
\keyword{attribute}
