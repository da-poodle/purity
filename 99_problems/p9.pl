% P09 (**) Pack consecutive duplicates of list elements into sublists.
%    If a list contains repeated elements they should be placed in separate sublists.
%
%    Example:
%    ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
pack([], []).
pack([A|T], [Dedupped|Result]) :-
    pack_similar([A|T], Dedupped, Remaining),
    pack(Remaining, Result).

pack_similar([A], [A], []).
pack_similar([A,A|T], [A|Similar], Remaining) :-
    pack_similar([A|T], Similar, Remaining).
pack_similar([A,B|T], [A], [B|T]) :-
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

