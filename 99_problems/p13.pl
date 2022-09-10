
% P13 (**) Run-length encoding of a list (direct solution).
%    Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton terms [1,X] by X.
%
%    Example:
%    ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[c(c(c(c(zero)))),a],b,[c(c(zero)),c],[c(c(zero)),a],d,[c(c(c(c(zero)))),e]]
%
encode_direct([],[]).
encode_direct([X|Xs],[Z|Zs]) :- count(X,Xs,Ys,c(zero),Z), encode_direct(Ys,Zs).

count(X,[],[],c(zero),X).
count(X,[],[],c(c(N)),[c(c(N)),X]). 
count(X,[Y|Ys],[Y|Ys],c(zero),X) :- pdif(X, Y).
count(X,[Y|Ys],[Y|Ys],c(c(N)),[c(c(N)),X]) :- pdif(X, Y).
count(X,[X|Xs],Ys,N,T) :- count(X,Xs,Ys,c(N),T).

pdif(A,B) :-
   pdif([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z], A, B). 

pdif(L, A, B) :- before_in_list(L, A, B).
pdif(L, A, B) :- before_in_list(L, B, A).

before_in_list([A|T],A,B) :-
    pmember(B,T).
before_in_list([_|T],A,B) :-
    before_in_list(T,A,B).

pmember(A,[A|_]).
pmember(A,[_|T]) :-
    pmember(A,T).

