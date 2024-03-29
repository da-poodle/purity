:- ensure_loaded(purity(purity)).
:- ensure_loaded(purity(plist)).
:- ensure_loaded(purity(pchar)).

test(plist_pcompare_empty_lists, A) :-
    pcompare([], [], C),
    pif(eq(C, '='), unify(A, true), unify(A, false)).

test(plist_pcompare_empty_one_element, A) :-
    pcompare([], [a], C),
    pif(eq(C, '<'), unify(A, true), unify(A, false)).

test(plist_pcompare_equal_elements, A) :-
    pcompare([a], [a], C),
    pif(eq(C, '='), unify(A, true), unify(A, false)).

test(plist_pcompare_lt_elements, A) :-
    pcompare([a], [b], C),
    pif(eq(C, '<'), unify(A, true), unify(A, false)).

test(plist_pcompare_gt_elements, A) :-
    pcompare([t], [b], C),
    pif(eq(C, '>'), unify(A, true), unify(A, false)).
