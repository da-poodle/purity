
btree(N, Tree) :-
    n_nodes(Tree, N),
    btree_(Tree, _).

btree_(l(x), c(zero)).
btree_(tl(x,l(x)), c(c(zero))).
btree_(tr(x,l(x)), c(c(zero))).
btree_(t(x,L,R), D) :-
    btree_(L, D1),
    btree_(R, D2),
    valid_depth(D,D1,D2).

valid_depth(c(zero), zero, zero).
valid_depth(c(c(zero)), c(zero), zero).
valid_depth(c(c(zero)), zero, c(zero)).
valid_depth(c(Z), c(A), c(B)) :-
    valid_depth(Z, A, B).

n_nodes(l(_), c(zero)).
n_nodes(tl(_,T), c(N)) :-
    n_nodes(T, N).
n_nodes(tr(_,T), c(N)) :-
    n_nodes(T, N).
n_nodes(t(_,L,R), c(N)) :-
    add(Nl,Nr,N),
    n_nodes(L,Nl),
    n_nodes(R,Nr).

add(zero, Z, Z).
add(c(X), Y, c(Z)) :- 
	add(X, Y, Z).
