\name{slice}
\alias{slice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
slice(...)
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
UseFunction(slice, "slice", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = c("1", 
    "1", "1"), node = c("argument", "argument", "argument"), 
        token = c("x", "pivot", "inclusive"), pattern = c(NA_character_, 
        NA_character_, NA_character_), default = c(NA, NA, "FALSE"
        )), .Names = c("paren.level", "node", "token", "pattern", 
    "default"), row.names = 2:4, class = "data.frame"), guard = function (x, 
        pivot, inclusive) 
    {
        is.null(dim(x))
    }, def = structure(function (x, pivot, inclusive) 
    {
        left <- x[1:pivot]
        right <- x[(pivot + as.numeric(!inclusive)):length(x)]
        list(left, right)
    }, topenv = "<environment: 0x159e350> ", name = "slice"), 
        signature = "slice(x, pivot, inclusive = FALSE) \%when\% {\n    is.null(dim(x))\n}", 
        body = "{\n    left <- x[1:pivot]\n    right <- x[(pivot + as.numeric(!inclusive)):length(x)]\n    list(left, right)\n}", 
        ellipsis = integer(0), fill.tokens = c("x", "pivot", 
        "inclusive"), fill.defaults = c(NA, NA, "FALSE"), accepts = 2:3, 
        type.index = 1), .Names = c("args", "guard", "def", "signature", 
    "body", "ellipsis", "fill.tokens", "fill.defaults", "accepts", 
    "type.index")), structure(list(args = structure(list(paren.level = c("1", 
    "1", "1"), node = c("argument", "argument", "argument"), 
        token = c("x", "pivot", "inclusive"), pattern = c(NA_character_, 
        NA_character_, NA_character_), default = c(NA, NA, "FALSE"
        )), .Names = c("paren.level", "node", "token", "pattern", 
    "default"), row.names = 2:4, class = "data.frame"), def = structure(function (x, 
        pivot, inclusive) 
    {
        left <- x[1:pivot, ]
        right <- x[(pivot + as.numeric(!inclusive)):nrow(x), 
            ]
        list(left, right)
    }, topenv = "<environment: 0x159e350> ", name = "slice"), 
        signature = "slice(x, pivot, inclusive = FALSE)", body = "{\n    left <- x[1:pivot, ]\n    right <- x[(pivot + as.numeric(!inclusive)):nrow(x), ]\n    list(left, right)\n}", 
        ellipsis = integer(0), fill.tokens = c("x", "pivot", 
        "inclusive"), fill.defaults = c(NA, NA, "FALSE"), accepts = 2:3, 
        type.index = 1), .Names = c("args", "def", "signature", 
    "body", "ellipsis", "fill.tokens", "fill.defaults", "accepts", 
    "type.index")), structure(list(args = structure(list(paren.level = c("1", 
    "1"), node = c("argument", "argument"), token = c("x", "expression"
    ), pattern = c(NA_character_, NA_character_), default = c(NA_character_, 
    NA_character_)), .Names = c("paren.level", "node", "token", 
    "pattern", "default"), row.names = 2:3, class = "data.frame"), 
        guard = function (x, expression) 
        {
            is.null(dim(x))
        }, def = structure(function (x, expression) 
        {
            left <- x[expression]
            right <- x[!expression]
            list(left, right)
        }, topenv = "<environment: 0x159e350> ", name = "slice"), 
        signature = "slice(x, expression) \%when\% {\n    is.null(dim(x))\n}", 
        body = "{\n    left <- x[expression]\n    right <- x[!expression]\n    list(left, right)\n}", 
        ellipsis = integer(0), fill.tokens = c("x", "expression"
        ), fill.defaults = c(NA_character_, NA_character_), accepts = c(2L, 
        2L), type.index = 2), .Names = c("args", "guard", "def", 
    "signature", "body", "ellipsis", "fill.tokens", "fill.defaults", 
    "accepts", "type.index")), structure(list(args = structure(list(
        paren.level = c("1", "1"), node = c("argument", "argument"
        ), token = c("x", "expression"), pattern = c(NA_character_, 
        NA_character_), default = c(NA_character_, NA_character_
        )), .Names = c("paren.level", "node", "token", "pattern", 
    "default"), row.names = 2:3, class = "data.frame"), def = structure(function (x, 
        expression) 
    {
        left <- x[expression, ]
        right <- x[!expression, ]
        list(left, right)
    }, topenv = "<environment: 0x159e350> ", name = "slice"), 
        signature = "slice(x, expression)", body = "{\n    left <- x[expression, ]\n    right <- x[!expression, ]\n    list(left, right)\n}", 
        ellipsis = integer(0), fill.tokens = c("x", "expression"
        ), fill.defaults = c(NA_character_, NA_character_), accepts = c(2L, 
        2L), type.index = 2), .Names = c("args", "def", "signature", 
    "body", "ellipsis", "fill.tokens", "fill.defaults", "accepts", 
    "type.index"))), types = list(structure(list(args = structure(list(
    paren.level = c("1", "1", "1"), node = c("argument", "argument", 
    "argument"), token = c("x", "pivot", "inclusive"), pattern = c(NA_character_, 
    NA_character_, NA_character_), default = c(NA_character_, 
    NA_character_, NA_character_)), .Names = c("paren.level", 
"node", "token", "pattern", "default"), row.names = 2:4, class = "data.frame"), 
    types = structure(list(line1 = c(1L, 1L, 1L, 1L), token = c("SYMBOL", 
    "SYMBOL", "SYMBOL", "SYMBOL"), text = c("a", "numeric", "logical", 
    "list")), .Names = c("line1", "token", "text"), class = "data.frame", row.names = c(21L, 
    24L, 28L, 32L)), signature = "slice(x, pivot, inclusive) \%::\% a:numeric:logical:list", 
    accepts = c(3L, 3L)), .Names = c("args", "types", "signature", 
"accepts")), structure(list(args = structure(list(paren.level = c("1", 
"1"), node = c("argument", "argument"), token = c("x", "expression"
), pattern = c(NA_character_, NA_character_), default = c(NA_character_, 
NA_character_)), .Names = c("paren.level", "node", "token", "pattern", 
"default"), row.names = 2:3, class = "data.frame"), types = structure(list(
    line1 = c(1L, 1L, 1L), token = c("SYMBOL", "SYMBOL", "SYMBOL"
    ), text = c("a", "logical", "list")), .Names = c("line1", 
"token", "text"), class = "data.frame", row.names = c(16L, 19L, 
23L)), signature = "slice(x, expression) \%::\% a:logical:list", 
    accepts = c(2L, 2L)), .Names = c("args", "types", "signature", 
"accepts"))), active.type = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
