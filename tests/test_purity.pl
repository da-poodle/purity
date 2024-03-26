:- ensure_loaded(purity(purity)).
:- ensure_loaded(purity(pchar)).

test(pif_eq, A) :-
    pif(eq(a, B), unify(A, true), unify(A, false)),
    unify(B, a).

test(pif_lt, A) :-
    pif(lt(a, b), unify(A, true), unify(A, false)).

test(pif_lt, A) :-
    pif(lt(b, a), unify(A, false), unify(A, true)).

test(pif_gt, A) :-
    pif(gt(b, a), unify(A, true), unify(A, false)).

test(pif_gt, A) :-
    pif(gt(a, b), unify(A, false), unify(A, true)).

test(pif_gte, A) :-
    pif(gte(a, b), unify(A, false), unify(A, true)).

test(pif_gte, A) :-
    pif(gte(b, a), unify(A, true), unify(A, false)).

test(pif_gte, A) :-
    pif(gte(a, a), unify(A, true), unify(A, false)).

test(pif_lte, A) :-
    pif(lte(a, b), unify(A, true), unify(A, false)).

test(pif_lte, A) :-
    pif(lte(b, a), unify(A, false), unify(A, true)).

test(pif_lte, A) :-
    pif(lte(a, a), unify(A, true), unify(A, false)).

test(pdif_dif, A) :-
    pif(pdif(a, b), unify(A, true), unify(A, false)).
    
test(pdif_eq, A) :-
    pif(pdif(a, a), unify(A, false), unify(A, true)).
 