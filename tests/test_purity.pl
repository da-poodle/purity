:- ensure_loaded(purity(purity)).
:- ensure_loaded(purity(pchar)).

test(pif_eq, A) :-
    pif(a = B, A = true, A = false),
    B = a.

test(pif_lt, A) :-
    pif(a < b, A = true, A = false).

test(pif_lt, A) :-
    pif(b < a, A = false, A = true).

test(pif_gt, A) :-
    pif(b > a, A = true, A = false).

test(pif_gt, A) :-
    pif(a > b, A = false, A = true).

test(pif_gte, A) :-
    pif(a >= b, A = false, A = true).

test(pif_gte, A) :-
    pif(b >= a, A = true, A = false).

test(pif_gte, A) :-
    pif(a >= a, A = true, A = false).

test(pif_lte, A) :-
    pif('<='(a, b), A = true, A = false).

test(pif_lte, A) :-
    pif('<='(b, a), A = false, A = true).

test(pif_lte, A) :-
    pif('<='(a, a), A = true, A = false).

test(pdif_dif, A) :-
    pif(pdif(a, b), A = true, A = false).
    
test(pdif_eq, A) :-
    pif(pdif(a, a), A = false, A = true).
 