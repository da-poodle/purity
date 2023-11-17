pcompare(A,B,C) :-
    compare(C, A, B).

% Check if value is a valid ptree
ptree(ptree_nil).
ptree(ptree(_,L,R)) :-
    ptree(L),
    ptree(R).

% Insert a value into a ptree
ptree_insert(ptree_nil, Val, ptree(Val, ptree_nil, ptree_nil)).
ptree_insert(ptree(V,L,R), Val, Result) :-
    pcompare(Val, V, C),
    ptree_insert(C, Val, ptree(V, L, R), Result).

ptree_insert(=, _, T, T).
ptree_insert(<, Val, ptree(V,L,R), ptree(V,Lr,R)) :-
    ptree_insert(L, Val, Lr).
ptree_insert(>, Val, ptree(V,L,R), ptree(V,L,Rr)) :-
    ptree_insert(R, Val, Rr).

% Remove and item from a ptree.
ptree_remove(ptree(Vn, L, R), Val, Result) :-
    pcompare(Val, Vn, C),
    ptree_remove(C, Val, ptree(Vn, L, R), Result).

ptree_remove(=, Val, ptree(Vn, L, R), Result) :-
    ptree_remove_node(L, R, Vn, Result)., R, Vn, R)
ptree_remove(<, Val, ptree(Vn, L, R), ptree(Vn, Lr, R)) :-
    ptree_remove(L, Val, Lr).
ptree_remove(>, Val, ptree(Vn, L, R), ptree(Vn, L, Rr) :-
    ptree_remove(R, Val, Rr).

ptree_remove_node(...).


% Check if a value is in a ptree
ptree_contains_t(ptree_nil, _, false).
ptree_contains_t(ptree(Val, L, R), V, Contains) :-
    pcompare(V, Val, C),
    ptree_contains_t(C, V, ptree(_, L, R), Contains).

ptree_contains_t(=, V, ptree(V,_,_), true).
ptree_contains_t(<, V, ptree(_,L,_), Contains) :-
    ptree_contains_t(L, V, Contains).
ptree_contains_t(>, V, ptree(_,_,R), Contains) :-
    ptree_contains_t(R, V, Contains).

% Convert a ptree to a list
ptree_list(Tree, List) :-
    ptree_list(Tree, [], List).

ptree_list(ptree_nil, A, A).
ptree_list(ptree(V, L, R), Acc, Rest) :-
    ptree_list(R, Acc, Left),
    ptree_list(L, [V|Left], Rest).

% Gets the size of a ptree.
ptree_size(Tree, Size) :-
    ptree_size(Tree, z, Size).

ptree_size(ptree_nil, S, S).
ptree_size(ptree(_,L,R), S, c(Size)) :-
    ptree_size(L, S, LSize),
    ptree_size(R, LSize, Size).
    
