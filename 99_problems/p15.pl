% P15 (**) Duplicate the elements of a list a given number of times.
%    Example:
%    ?- dupli([a,b,c],c(c(c(zero))),X).
%    X = [a,a,a,b,b,b,c,c,c]
%
%    What are the results of the goal:
%    ?- dupli(X,3,Y).
%
dupli([],_,[]).
dupli([X|Xs],N,Ys) :- duplix(N,X,Ys-T), dupli(Xs,N,T).

duplix(zero,_,T-T).
duplix(c(N),X,[X|R]-T) :-
    duplix(N,X,R-T).

