\name{is.empty}
\alias{is.empty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
is.empty(...)
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
UseFunction(is.empty, "is.empty", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = "1", node = "argument", 
        token = "x", pattern = NA_character_, default = NA_character_), .Names = c("paren.level", 
    "node", "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        def = structure(function (x) 
        {
            length(x) < 1
        }, topenv = "<environment: 0x159e350> ", name = "is.empty"), 
        signature = "is.empty(x)", body = "{\n    length(x) < 1\n}", 
        ellipsis = integer(0), fill.tokens = "x", fill.defaults = NA_character_, 
        accepts = c(1L, 1L), type.index = 1), .Names = c("args", 
    "def", "signature", "body", "ellipsis", "fill.tokens", "fill.defaults", 
    "accepts", "type.index"))), types = list(structure(list(args = structure(list(
    paren.level = "1", node = "argument", token = "x", pattern = NA_character_, 
    default = NA_character_), .Names = c("paren.level", "node", 
"token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
    types = structure(list(line1 = c(1L, 1L), token = c("SYMBOL", 
    "SYMBOL"), text = c("a", "logical")), .Names = c("line1", 
    "token", "text"), class = "data.frame", row.names = c(11L, 
    14L)), signature = "is.empty(x) \%::\% a:logical", accepts = c(1L, 
    1L)), .Names = c("args", "types", "signature", "accepts"))), active.type = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
