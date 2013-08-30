\name{fold}
\alias{fold}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fold(...)
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
UseFunction(fold, "fold", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = c("1", 
    "1", "1"), node = c("argument", "argument", "argument"), 
        token = c("fn", ".lambda_2", "acc"), pattern = c(NA, 
        "EMPTY", NA), default = c(NA_character_, NA_character_, 
        NA_character_)), .Names = c("paren.level", "node", "token", 
    "pattern", "default"), row.names = 2:4, class = "data.frame"), 
        guard = function (fn, .lambda_2, acc) 
        {
            length(.lambda_2) == 0
        }, def = structure(function (fn, .lambda_2, acc) 
        {
            acc
        }, topenv = "<environment: 0x159e350> ", name = "fold"), 
        signature = "fold(fn, EMPTY, acc)", body = "acc", ellipsis = integer(0), 
        fill.tokens = c("fn", ".lambda_2", "acc"), fill.defaults = c(NA_character_, 
        NA_character_, NA_character_), accepts = c(3L, 3L)), .Names = c("args", 
    "guard", "def", "signature", "body", "ellipsis", "fill.tokens", 
    "fill.defaults", "accepts")), structure(list(args = structure(list(
        paren.level = c("1", "1", "1"), node = c("argument", 
        "argument", "argument"), token = c("fn", "x", "acc"), 
        pattern = c(NA_character_, NA_character_, NA_character_
        ), default = c(NA_character_, NA_character_, NA_character_
        )), .Names = c("paren.level", "node", "token", "pattern", 
    "default"), row.names = 2:4, class = "data.frame"), guard = function (fn, 
        x, acc) 
    {
        is.null(dim(x))
    }, def = structure(function (fn, x, acc) 
    {
        fold(fn, x[-1], f(x[[1]], acc))
    }, topenv = "<environment: 0x159e350> ", name = "fold"), 
        signature = "fold(fn, x, acc) \%when\% {\n    is.null(dim(x))\n}", 
        body = "fold(fn, x[-1], f(x[[1]], acc))", ellipsis = integer(0), 
        fill.tokens = c("fn", "x", "acc"), fill.defaults = c(NA_character_, 
        NA_character_, NA_character_), accepts = c(3L, 3L)), .Names = c("args", 
    "guard", "def", "signature", "body", "ellipsis", "fill.tokens", 
    "fill.defaults", "accepts")), structure(list(args = structure(list(
        paren.level = c("1", "1", "1"), node = c("argument", 
        "argument", "argument"), token = c("fn", "x", "acc"), 
        pattern = c(NA_character_, NA_character_, NA_character_
        ), default = c(NA_character_, NA_character_, NA_character_
        )), .Names = c("paren.level", "node", "token", "pattern", 
    "default"), row.names = 2:4, class = "data.frame"), def = structure(function (fn, 
        x, acc) 
    {
        fold(fn, x[, -1, drop = FALSE], f(x[, 1], acc))
    }, topenv = "<environment: 0x159e350> ", name = "fold"), 
        signature = "fold(fn, x, acc)", body = "fold(fn, x[, -1, drop = FALSE], f(x[, 1], acc))", 
        ellipsis = integer(0), fill.tokens = c("fn", "x", "acc"
        ), fill.defaults = c(NA_character_, NA_character_, NA_character_
        ), accepts = c(3L, 3L)), .Names = c("args", "def", "signature", 
    "body", "ellipsis", "fill.tokens", "fill.defaults", "accepts"
    ))), types = list())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
