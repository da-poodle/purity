% P16 (**) Drop every N'th element from a list.
%    Example:
%    ?- drop([a,b,c,d,e,f,g,h,i,k],c(c(c(zero))),X).
%    X = [a,b,d,e,g,h,k]
%
drop(Xs,N,Ys) :- drop(Xs,N,N,Ys).

drop([],_,_,[]).
drop([_|Xs],N,c(zero),Ys) :- 
    drop(Xs,N,N,Ys).
drop([X|Xs],N,c(c(Nc)),[X|Ys]) :- 
    drop(Xs,N,c(Nc),Ys).

