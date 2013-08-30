\name{maprange}
\alias{maprange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
maprange(...)
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
UseFunction(maprange, "maprange", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = c("1", 
    "1", "1", "1"), node = c("argument", "argument", "argument", 
    "argument"), token = c("fn", "x", "window", "pad"), pattern = c(NA_character_, 
    NA_character_, NA_character_, NA_character_), default = c(NA, 
    NA, NA, "FALSE")), .Names = c("paren.level", "node", "token", 
    "pattern", "default"), row.names = 2:5, class = "data.frame"), 
        guard = function (fn, x, window, pad) 
        {
            is.null(dim(x))
        }, def = structure(function (fn, x, window, pad) 
        {
            y <- sapply((window):length(x), function(idx) fn(x[(idx - 
                window + 1):idx]))
            onlyif(function(z) pad(z, window - 1), y, pad)
        }, topenv = "<environment: 0x159e350> ", name = "maprange"), 
        signature = "maprange(fn, x, window, pad = FALSE) \%when\% {\n    is.null(dim(x))\n}", 
        body = "{\n    y <- sapply((window):length(x), function(idx) fn(x[(idx - \n        window + 1):idx]))\n    onlyif(function(z) pad(z, window - 1), y, pad)\n}", 
        ellipsis = integer(0), fill.tokens = c("fn", "x", "window", 
        "pad"), fill.defaults = c(NA, NA, NA, "FALSE"), accepts = 3:4), .Names = c("args", 
    "guard", "def", "signature", "body", "ellipsis", "fill.tokens", 
    "fill.defaults", "accepts")), structure(list(args = structure(list(
        paren.level = c("1", "1", "1", "1"), node = c("argument", 
        "argument", "argument", "argument"), token = c("fn", 
        "x", "window", "pad"), pattern = c(NA_character_, NA_character_, 
        NA_character_, NA_character_), default = c(NA, NA, NA, 
        "FALSE")), .Names = c("paren.level", "node", "token", 
    "pattern", "default"), row.names = 2:5, class = "data.frame"), 
        def = structure(function (fn, x, window, pad) 
        {
            sapply(1:ncol(x), function(ydx) maprange(fn, x[, 
                ydx], window, pad))
        }, topenv = "<environment: 0x159e350> ", name = "maprange"), 
        signature = "maprange(fn, x, window, pad = FALSE)", body = "{\n    sapply(1:ncol(x), function(ydx) maprange(fn, x[, ydx], window, \n        pad))\n}", 
        ellipsis = integer(0), fill.tokens = c("fn", "x", "window", 
        "pad"), fill.defaults = c(NA, NA, NA, "FALSE"), accepts = 3:4), .Names = c("args", 
    "def", "signature", "body", "ellipsis", "fill.tokens", "fill.defaults", 
    "accepts"))), types = list())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
