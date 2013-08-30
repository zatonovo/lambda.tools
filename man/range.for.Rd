\name{range.for}
\alias{range.for}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
range.for(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (...) 
UseFunction(range.for, "range.for", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = c("1", 
    "1"), node = c("argument", "argument"), token = c("value", 
    "series"), pattern = c(NA_character_, NA_character_), default = c(NA_character_, 
    NA_character_)), .Names = c("paren.level", "node", "token", 
    "pattern", "default"), row.names = 2:3, class = "data.frame"), 
        def = structure(function (value, series) 
        {
            x <- segment(series, TRUE)
            idx <- 1:nrow(x)
            idx.inf <- (is.na(x$a) | x$a != value) & (!is.na(x$b) & 
                x$b == value)
            idx.sup <- (!is.na(x$a) & x$a == value) & (is.na(x$b) | 
                x$b != value)
            data.frame(min = idx[idx.inf], max = idx[idx.sup] - 
                1)
        }, topenv = "<environment: 0x159e350> ", name = "range.for"), 
        signature = "range.for(value, series)", body = "{\n    x <- segment(series, TRUE)\n    idx <- 1:nrow(x)\n    idx.inf <- (is.na(x$a) | x$a != value) & (!is.na(x$b) & x$b == \n        value)\n    idx.sup <- (!is.na(x$a) & x$a == value) & (is.na(x$b) | x$b != \n        value)\n    data.frame(min = idx[idx.inf], max = idx[idx.sup] - 1)\n}", 
        ellipsis = integer(0), fill.tokens = c("value", "series"
        ), fill.defaults = c(NA_character_, NA_character_), accepts = c(2L, 
        2L)), .Names = c("args", "def", "signature", "body", 
    "ellipsis", "fill.tokens", "fill.defaults", "accepts"))), types = list())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
