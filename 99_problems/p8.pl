% P08 (**) Eliminate consecutive duplicates of list elements.
%    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
%
%    Example:
%    ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [a,b,c,a,d,e]
compress([], []).
compress([A|T], [Dedupped|Result]) :-
    remove_seq([A|T], Dedupped, Remaining),
    compress(Remaining, Result).

remove_seq([A], A, []).
remove_seq([A,A|T], A, Remaining) :-
    remove_seq([A|T], _, Remaining).
remove_seq([A,B|T], A, [B|T]) :-
    pdif(A,B).

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

