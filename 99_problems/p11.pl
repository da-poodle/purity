% P11 (*) Modified run-length encoding.
%    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as [N,E] terms.
%
%    Example:
%    ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_modified(List, Encoded) :-
    pack(List, Packed),
    maplist(encoded_counted_mod, Packed, Encoded).

encoded_counted_mod(Packed, R) :-
    encoded_counted_mod(Packed, D, N),
    encode_by_type(N, D, R).

encode_by_type(c(zero), D, D).
encode_by_type(c(c(N)), D, [c(c(N)), D]).

encoded_counted_mod([A], A, c(zero)).
encoded_counted_mod([A|T], A, c(c(N))) :-
    encoded_counted_mod(T, A, c(N)).

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

