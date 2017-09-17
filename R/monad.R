# :vim set filetype=R
#' Provide monadi primitives
#'
#' These functions implement basic monads as seen in Haskell.
#'
#' @section Usage:
#' Maybe(a) %:=% a
#' Just(a) %:=% Maybe(a)
#' Nothing() %:=% Maybe(NA)
#' 
#' unit(x) %::% . : Maybe
#' unit(x) %as% Just(x)
#' 
#' Bind operator
#' m %>>=% g %::% Nothing : Function : Maybe
#' m %>>=% g %as% m
#' 
#' m %>>=% g %::% Just : Function : Maybe
#' m %>>=% g %as% g(m)
#'
#' Regular composition f(g(x))
#' f %.% g %:=% function(...) f(g(...))
#' 
#' Monad composition g(f(x))
#' f %>=>% g %:=% { function(x) f(x) %>>=% g }
#' 
#' Monad composition f(g(x))
#' f %<=<% g %:=% { function(x) g(x) %>>=% f }
#'
#' @section Details:
#' Monads are types that carry context. Monad operators enable chaining
#' monadic operations together.
#'
#' @name monad
#' @aliases Maybe Just Nothing unit %>>=% %>=>% %<=<% %.% 
#' @return Usually a monad
#' @examples
#' require(lambda.r)
#' safelog(x) %::% numeric : Maybe
#' safelog(x) %when% { x <= 0 } %as% Nothing()
#' safelog(x) %:=% Just(log(x))
#' 
#' safesqrt(x) %::% numeric : Maybe
#' safesqrt(x) %when% { x <= 0 } %as% Nothing()
#' safesqrt(x) %:=% Just(sqrt(x))
#' 
#' safelogsqrt <- safelog %<=<% safesqrt
#' safelogsqrt(-2)
#' safelogsqrt(2)

Maybe(a) %:=% a
Just(a) %:=% Maybe(a)
Nothing() %:=% Maybe(NA)

unit(x) %::% . : Maybe
unit(x) %as% Just(x)

# Bind operator
m %>>=% g %::% Nothing : Function : Maybe
m %>>=% g %as% m

m %>>=% g %::% Just : Function : Maybe
m %>>=% g %as% g(m)


# Regular composition f(g(x))
f %.% g %:=% function(...) f(g(...))

# Monad composition g(f(x))
f %>=>% g %:=% { function(x) f(x) %>>=% g }

# Monad composition f(g(x))
f %<=<% g %:=% { function(x) g(x) %>>=% f }


