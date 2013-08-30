\name{is.bad}
\alias{is.bad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
is.bad(...)
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
UseFunction(is.bad, "is.bad", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = "1", node = "argument", 
        token = "x", pattern = NA_character_, default = NA_character_), .Names = c("paren.level", 
    "node", "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        guard = function (x) 
        {
            is.null(x)
        }, def = structure(function (x) 
        {
            TRUE
        }, topenv = "<environment: 0x159e350> ", name = "is.bad"), 
        signature = "is.bad(x) \%when\% {\n    is.null(x)\n}", 
        body = "TRUE", ellipsis = integer(0), fill.tokens = "x", 
        fill.defaults = NA_character_, accepts = c(1L, 1L), type.index = 1), .Names = c("args", 
    "guard", "def", "signature", "body", "ellipsis", "fill.tokens", 
    "fill.defaults", "accepts", "type.index")), structure(list(
        args = structure(list(paren.level = "1", node = "argument", 
            token = "x", pattern = NA_character_, default = NA_character_), .Names = c("paren.level", 
        "node", "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        guard = function (x) 
        {
            is.empty(x)
        }, def = structure(function (x) 
        {
            TRUE
        }, topenv = "<environment: 0x159e350> ", name = "is.bad"), 
        signature = "is.bad(x) \%when\% {\n    is.empty(x)\n}", 
        body = "TRUE", ellipsis = integer(0), fill.tokens = "x", 
        fill.defaults = NA_character_, accepts = c(1L, 1L), type.index = 1), .Names = c("args", 
    "guard", "def", "signature", "body", "ellipsis", "fill.tokens", 
    "fill.defaults", "accepts", "type.index")), structure(list(
        args = structure(list(paren.level = "1", node = "argument", 
            token = "x", pattern = NA_character_, default = NA_character_), .Names = c("paren.level", 
        "node", "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        def = structure(function (x) 
        {
            lapply(x, is.bad)
        }, topenv = "<environment: 0x159e350> ", name = "is.bad"), 
        signature = "is.bad(x)", body = "{\n    lapply(x, is.bad)\n}", 
        ellipsis = integer(0), fill.tokens = "x", fill.defaults = NA_character_, 
        accepts = c(1L, 1L), type.index = 2), .Names = c("args", 
    "def", "signature", "body", "ellipsis", "fill.tokens", "fill.defaults", 
    "accepts", "type.index")), structure(list(args = structure(list(
        paren.level = "1", node = "argument", token = "x", pattern = NA_character_, 
        default = NA_character_), .Names = c("paren.level", "node", 
    "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        def = structure(function (x) 
        {
            sapply(x, is.bad)
        }, topenv = "<environment: 0x159e350> ", name = "is.bad"), 
        signature = "is.bad(x)", body = "{\n    sapply(x, is.bad)\n}", 
        ellipsis = integer(0), fill.tokens = "x", fill.defaults = NA_character_, 
        accepts = c(1L, 1L), type.index = 3), .Names = c("args", 
    "def", "signature", "body", "ellipsis", "fill.tokens", "fill.defaults", 
    "accepts", "type.index")), structure(list(args = structure(list(
        paren.level = "1", node = "argument", token = "x", pattern = NA_character_, 
        default = NA_character_), .Names = c("paren.level", "node", 
    "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        def = structure(function (x) 
        {
            apply(x, 1, is.bad)
        }, topenv = "<environment: 0x159e350> ", name = "is.bad"), 
        signature = "is.bad(x)", body = "{\n    apply(x, 1, is.bad)\n}", 
        ellipsis = integer(0), fill.tokens = "x", fill.defaults = NA_character_, 
        accepts = c(1L, 1L), type.index = 4), .Names = c("args", 
    "def", "signature", "body", "ellipsis", "fill.tokens", "fill.defaults", 
    "accepts", "type.index")), structure(list(args = structure(list(
        paren.level = "1", node = "argument", token = "x", pattern = NA_character_, 
        default = NA_character_), .Names = c("paren.level", "node", 
    "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        def = structure(function (x) 
        {
            is.na(x)
        }, topenv = "<environment: 0x159e350> ", name = "is.bad"), 
        signature = "is.bad(x)", body = "{\n    is.na(x)\n}", 
        ellipsis = integer(0), fill.tokens = "x", fill.defaults = NA_character_, 
        accepts = c(1L, 1L), type.index = 1L), .Names = c("args", 
    "def", "signature", "body", "ellipsis", "fill.tokens", "fill.defaults", 
    "accepts", "type.index"))), types = list(structure(list(args = structure(list(
    paren.level = "1", node = "argument", token = "x", pattern = NA_character_, 
    default = NA_character_), .Names = c("paren.level", "node", 
"token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
    types = structure(list(line1 = c(1L, 1L), token = c("SYMBOL", 
    "SYMBOL"), text = c("a", "logical")), .Names = c("line1", 
    "token", "text"), class = "data.frame", row.names = c(11L, 
    14L)), signature = "is.bad(x) \%::\% a:logical", accepts = c(1L, 
    1L)), .Names = c("args", "types", "signature", "accepts")), 
    structure(list(args = structure(list(paren.level = "1", node = "argument", 
        token = "x", pattern = NA_character_, default = NA_character_), .Names = c("paren.level", 
    "node", "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        types = structure(list(line1 = c(1L, 1L), token = c("SYMBOL", 
        "SYMBOL"), text = c("list", "list")), .Names = c("line1", 
        "token", "text"), class = "data.frame", row.names = c(11L, 
        14L)), signature = "is.bad(x) \%::\% list:list", accepts = c(1L, 
        1L)), .Names = c("args", "types", "signature", "accepts"
    )), structure(list(args = structure(list(paren.level = "1", 
        node = "argument", token = "x", pattern = NA_character_, 
        default = NA_character_), .Names = c("paren.level", "node", 
    "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        types = structure(list(line1 = c(1L, 1L), token = c("SYMBOL", 
        "SYMBOL"), text = c("data.frame", "matrix")), .Names = c("line1", 
        "token", "text"), class = "data.frame", row.names = c(11L, 
        14L)), signature = "is.bad(x) \%::\% data.frame:matrix", 
        accepts = c(1L, 1L)), .Names = c("args", "types", "signature", 
    "accepts")), structure(list(args = structure(list(paren.level = "1", 
        node = "argument", token = "x", pattern = NA_character_, 
        default = NA_character_), .Names = c("paren.level", "node", 
    "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        types = structure(list(line1 = c(1L, 1L), token = c("SYMBOL", 
        "SYMBOL"), text = c("matrix", "matrix")), .Names = c("line1", 
        "token", "text"), class = "data.frame", row.names = c(11L, 
        14L)), signature = "is.bad(x) \%::\% matrix:matrix", accepts = c(1L, 
        1L)), .Names = c("args", "types", "signature", "accepts"
    ))), active.type = 1L)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
