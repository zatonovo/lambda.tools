\name{pad}
\alias{pad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pad(...)
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
UseFunction(pad, "pad", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = c("1", 
    "1", "1", "1"), node = c("argument", "argument", "argument", 
    "argument"), token = c("x", "head", "tail", "default"), pattern = c(NA_character_, 
    NA_character_, NA_character_, NA_character_), default = c(NA, 
    NA, "0", "NA")), .Names = c("paren.level", "node", "token", 
    "pattern", "default"), row.names = 2:5, class = "data.frame"), 
        def = structure(function (x, head, tail, default) 
        {
            c(rep(default, head), x, rep(default, tail))
        }, topenv = "<environment: 0x159e350> ", name = "pad"), 
        signature = "pad(x, head, tail = 0, default = NA)", body = "c(rep(default, head), x, rep(default, tail))", 
        ellipsis = integer(0), fill.tokens = c("x", "head", "tail", 
        "default"), fill.defaults = c(NA, NA, "0", "NA"), accepts = c(2L, 
        4L)), .Names = c("args", "def", "signature", "body", 
    "ellipsis", "fill.tokens", "fill.defaults", "accepts"))), types = list())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
