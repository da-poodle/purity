% P17 (*) Split a list into two parts; the length of the first part is given.
%    Do not use any predefined predicates.
%
%    Example:
%    ?- split([a,b,c,d,e,f,g,h,i,k],c(c(c(zero))),L1,L2).
%    L1 = [a,b,c]
%    L2 = [d,e,f,g,h,i,k]
%
split(L,zero,[],L).
split([X|Xs],c(N),[X|Ys],L2) :- 
    split(Xs,N,Ys,L2).

