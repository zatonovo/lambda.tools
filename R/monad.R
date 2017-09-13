Maybe(a) %:=% a
Just(a) %:=% Maybe(a)
Nothing() %:=% Maybe(NA)
      
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


