\name{segment}
\alias{segment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
segment(...)
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
UseFunction(segment, "segment", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = c("1", 
    "1"), node = c("argument", "argument"), token = c("x", "pad"
    ), pattern = c(NA_character_, NA_character_), default = c(NA, 
    "FALSE")), .Names = c("paren.level", "node", "token", "pattern", 
    "default"), row.names = 2:3, class = "data.frame"), def = structure(function (x, 
        pad) 
    {
        x <- onlyif(function(y) pad(y, 1, 1), x, pad)
        data.frame(a = x[1:(length(x) - 1)], b = x[2:length(x)])
    }, topenv = "<environment: 0x159e350> ", name = "segment"), 
        signature = "segment(x, pad = FALSE)", body = "{\n    x <- onlyif(function(y) pad(y, 1, 1), x, pad)\n    data.frame(a = x[1:(length(x) - 1)], b = x[2:length(x)])\n}", 
        ellipsis = integer(0), fill.tokens = c("x", "pad"), fill.defaults = c(NA, 
        "FALSE"), accepts = 1:2), .Names = c("args", "def", "signature", 
    "body", "ellipsis", "fill.tokens", "fill.defaults", "accepts"
    ))), types = list())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
