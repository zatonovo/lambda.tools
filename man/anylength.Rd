\name{anylength}
\alias{anylength}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
anylength(...)
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
UseFunction(anylength, "anylength", ...), class = "lambdar.fun", variants = list(
    structure(list(args = structure(list(paren.level = "1", node = "argument", 
        token = "data", pattern = NA_character_, default = NA_character_), .Names = c("paren.level", 
    "node", "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        guard = function (data) 
        {
            !is.null(nrow(data))
        }, def = structure(function (data) 
        {
            nrow(data)
        }, topenv = "<environment: 0x159e350> ", name = "anylength"), 
        signature = "anylength(data) \%when\% {\n    !is.null(nrow(data))\n}", 
        body = "nrow(data)", ellipsis = integer(0), fill.tokens = "data", 
        fill.defaults = NA_character_, accepts = c(1L, 1L)), .Names = c("args", 
    "guard", "def", "signature", "body", "ellipsis", "fill.tokens", 
    "fill.defaults", "accepts")), structure(list(args = structure(list(
        paren.level = "1", node = "argument", token = "data", 
        pattern = NA_character_, default = NA_character_), .Names = c("paren.level", 
    "node", "token", "pattern", "default"), row.names = 2L, class = "data.frame"), 
        def = structure(function (data) 
        {
            length(data)
        }, topenv = "<environment: 0x159e350> ", name = "anylength"), 
        signature = "anylength(data)", body = "length(data)", 
        ellipsis = integer(0), fill.tokens = "data", fill.defaults = NA_character_, 
        accepts = c(1L, 1L)), .Names = c("args", "def", "signature", 
    "body", "ellipsis", "fill.tokens", "fill.defaults", "accepts"
    ))), types = list())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
