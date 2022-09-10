% P10 (*) Run-length encoding of a list.
%    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.
%
%    Example:
%    ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]    
%
encode(List, Encoded) :-
    pack(List, Packed),
    maplist(encoded_counted, Packed, Encoded).

encoded_counted(Packed, [N, A]) :-
    encoded_counted(Packed, A, N).

encoded_counted([A], A, c(zero)).
encoded_counted([A|T], A, c(c(N))) :-
    encoded_counted(T, A, c(N)).

% The pack implemenation from P09
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

