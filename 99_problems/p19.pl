% P19 (**) Rotate a list N places to the left.
%    Examples:
%    ?- rotate(left(c(c(c(zero))), [a,b,c,d,e,f,g,h],X).
%    X = [d,e,f,g,h,a,b,c]
%
%    ?- rotate(right(c(c(zero)), [a,b,c,d,e,f,g,h],X).
%    X = [g,h,a,b,c,d,e,f]
%
%    Hint: Use the predefined predicates length/2 and append/3, as well as the result of problem P17.
rotate(left(N), List, Result) :-
    split(List, N, L1, L2),
    pappend(L2, L1, Result).
rotate(right(N), List, Result) :-
    plength(List, Len),
    add(NDiff, N, Len) ,
    rotate(left(NDiff), List, Result).

split(L,zero,[],L).
split([X|Xs],c(N),[X|Ys],L2) :- 
    split(Xs,N,Ys,L2).

pappend([], A, A).
pappend([A|B], C, [A|D]) :-
    pappend(B, C, D).

plength([], zero).
plength([_|T], c(Z)) :- plength(T, Z).

% add(UnaryA, UnaryB, Sum).
add(zero, Z, Z).
add(c(X), Y, c(Z)) :- 
	add(X, Y, Z).





