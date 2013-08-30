\name{chomp}
\alias{chomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chomp(...)
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
UseFunction(chomp, "chomp", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = c("1", 
    "1", "1"), node = c("argument", "argument", "argument"), 
        token = c("x", "head", "tail"), pattern = c(NA_character_, 
        NA_character_, NA_character_), default = c(NA, "1", "1"
        )), .Names = c("paren.level", "node", "token", "pattern", 
    "default"), row.names = 2:4, class = "data.frame"), guard = function (x, 
        head, tail) 
    {
        is.null(dim(x))
    }, def = structure(function (x, head, tail) 
    {
        x[(1 + head):(length(x) - tail)]
    }, topenv = "<environment: 0x159e350> ", name = "chomp"), 
        signature = "chomp(x, head = 1, tail = 1) \%when\% {\n    is.null(dim(x))\n}", 
        body = "{\n    x[(1 + head):(length(x) - tail)]\n}", 
        ellipsis = integer(0), fill.tokens = c("x", "head", "tail"
        ), fill.defaults = c(NA, "1", "1"), accepts = c(1L, 3L
        )), .Names = c("args", "guard", "def", "signature", "body", 
    "ellipsis", "fill.tokens", "fill.defaults", "accepts")), 
    structure(list(args = structure(list(paren.level = c("1", 
    "1", "1"), node = c("argument", "argument", "argument"), 
        token = c("x", "head", "tail"), pattern = c(NA_character_, 
        NA_character_, NA_character_), default = c(NA, "1", "1"
        )), .Names = c("paren.level", "node", "token", "pattern", 
    "default"), row.names = 2:4, class = "data.frame"), def = structure(function (x, 
        head, tail) 
    {
        x[(1 + head):(length(x) - tail), ]
    }, topenv = "<environment: 0x159e350> ", name = "chomp"), 
        signature = "chomp(x, head = 1, tail = 1)", body = "{\n    x[(1 + head):(length(x) - tail), ]\n}", 
        ellipsis = integer(0), fill.tokens = c("x", "head", "tail"
        ), fill.defaults = c(NA, "1", "1"), accepts = c(1L, 3L
        )), .Names = c("args", "def", "signature", "body", "ellipsis", 
    "fill.tokens", "fill.defaults", "accepts"))), types = list())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
