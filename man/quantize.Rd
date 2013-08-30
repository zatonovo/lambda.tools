\name{quantize}
\alias{quantize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
quantize(...)
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
UseFunction(quantize, "quantize", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = c("1", 
    "1", "1"), node = c("argument", "argument", "argument"), 
        token = c("x", "bins", "metric"), pattern = c(NA_character_, 
        NA_character_, NA_character_), default = c(NA, "c ( - 1 , 0 , 1 )", 
        "function ( a , b ) abs ( a - b )")), .Names = c("paren.level", 
    "node", "token", "pattern", "default"), row.names = 2:4, class = "data.frame"), 
        def = structure(function (x, bins, metric) 
        {
            ds <- sapply(bins, function(b) metric(x, b))
            if (is.null(dim(ds))) 
                ds <- t(ds)
            apply(ds, 1, function(d) item(bins, which.min(d)))
        }, topenv = "<environment: 0x159e350> ", name = "quantize"), 
        signature = "quantize(x, bins = c(-1, 0, 1), metric = function(a, b) abs(a - \n    b))", 
        body = "{\n    ds <- sapply(bins, function(b) metric(x, b))\n    if (is.null(dim(ds))) \n        ds <- t(ds)\n    apply(ds, 1, function(d) item(bins, which.min(d)))\n}", 
        ellipsis = integer(0), fill.tokens = c("x", "bins", "metric"
        ), fill.defaults = c(NA, "c ( - 1 , 0 , 1 )", "function ( a , b ) abs ( a - b )"
        ), accepts = c(1L, 3L)), .Names = c("args", "def", "signature", 
    "body", "ellipsis", "fill.tokens", "fill.defaults", "accepts"
    ))), types = list())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
