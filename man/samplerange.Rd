\name{samplerange}
\alias{samplerange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
samplerange(...)
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
UseFunction(samplerange, "samplerange", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = c("1", 
    "1", "1", "1"), node = c("argument", "argument", "argument", 
    "argument"), token = c("x", "size", "window", "..."), pattern = c(NA_character_, 
    NA_character_, NA_character_, NA_character_), default = c(NA_character_, 
    NA_character_, NA_character_, NA_character_)), .Names = c("paren.level", 
    "node", "token", "pattern", "default"), row.names = 2:5, class = "data.frame"), 
        guard = function (x, size, window, ...) 
        {
            is.null(dim(x)) & window < length(x)
        }, def = structure(function (x, size, window, ...) 
        {
            count <- length(x) - window + 1
            samples <- sample.int(count, size, ...)
            t(sapply(samples, function(s) x[s:(s + window - 1)]))
        }, topenv = "<environment: 0x159e350> ", name = "samplerange"), 
        signature = "samplerange(x, size, window, ...) \%when\% {\n    is.null(dim(x))\n    window < length(x)\n}", 
        body = "{\n    count <- length(x) - window + 1\n    samples <- sample.int(count, size, ...)\n    t(sapply(samples, function(s) x[s:(s + window - 1)]))\n}", 
        ellipsis = 4L, fill.tokens = c("x", "size", "window"), 
        fill.defaults = c(NA_character_, NA_character_, NA_character_
        ), accepts = c(3, Inf)), .Names = c("args", "guard", 
    "def", "signature", "body", "ellipsis", "fill.tokens", "fill.defaults", 
    "accepts")), structure(list(args = structure(list(paren.level = c("1", 
    "1", "1", "1"), node = c("argument", "argument", "argument", 
    "argument"), token = c("x", "size", "window", "..."), pattern = c(NA_character_, 
    NA_character_, NA_character_, NA_character_), default = c(NA_character_, 
    NA_character_, NA_character_, NA_character_)), .Names = c("paren.level", 
    "node", "token", "pattern", "default"), row.names = 2:5, class = "data.frame"), 
        guard = function (x, size, window, ...) 
        {
            window < length(x)
        }, def = structure(function (x, size, window, ...) 
        {
            count <- nrow(x) - window + 1
            samples <- sample.int(count, size, ...)
            lapply(samples, function(s) x[s:(s + window - 1), 
                ])
        }, topenv = "<environment: 0x159e350> ", name = "samplerange"), 
        signature = "samplerange(x, size, window, ...) \%when\% {\n    window < length(x)\n}", 
        body = "{\n    count <- nrow(x) - window + 1\n    samples <- sample.int(count, size, ...)\n    lapply(samples, function(s) x[s:(s + window - 1), ])\n}", 
        ellipsis = 4L, fill.tokens = c("x", "size", "window"), 
        fill.defaults = c(NA_character_, NA_character_, NA_character_
        ), accepts = c(3, Inf)), .Names = c("args", "guard", 
    "def", "signature", "body", "ellipsis", "fill.tokens", "fill.defaults", 
    "accepts"))), types = list())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
