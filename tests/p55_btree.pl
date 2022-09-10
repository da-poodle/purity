
btree(N, Tree) :-
    n_nodes(Tree, N),
    btree_(Tree, _).

btree_(nil, zero).
btree_(t(x,L,R), D) :-
    btree_(L, D1),
    btree_(R, D2),
    valid_depth(D,D1,D2).

valid_depth(c(zero), zero, zero).
valid_depth(c(c(zero)), c(zero), zero).
valid_depth(c(c(zero)), zero, c(zero)).
valid_depth(c(Z), c(A), c(B)) :-
    valid_depth(Z, A, B).

n_nodes(nil, zero).
n_nodes(t(_,L,R), c(N)) :-
    n_nodes(L,Nl),
    n_nodes(R,Nr),
    add(Nl,Nr,N).

add(zero, Z, Z) :- unary(Z).
add(c(X), Y, c(Z)) :- 
	add(X, Y, Z).

unary(zero).
unary(c(Z)) :- unary(Z).