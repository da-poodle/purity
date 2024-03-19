:- ensure_loaded(purity(purity)).
:- ensure_loaded(purity(pchar)).

test(pif:eq, A) :-
    pif(a = B, A = true, A = false),
    B = a.

test(pif:lt, A) :-
    pif(a < b, A = true, A = false).

test(pif:lt, A) :-
    pif(b < a, A = false, A = true).

test(pif:gt, A) :-
    pif(b > a, A = true, A = false).

test(pif:gt, A) :-
    pif(a > b, A = false, A = true).

test(pif:gte, A) :-
    pif(a >= b, A = false, A = true).

test(pif:gte, A) :-
    pif(b >= a, A = true, A = false).

test(pif:gte, A) :-
    pif(a >= a, A = true, A = false).

test(pif:lte, A) :-
    pif('<='(a, b), A = true, A = false).

test(pif:lte, A) :-
    pif('<='(b, a), A = false, A = true).

test(pif:lte, A) :-
    pif('<='(a, a), A = true, A = false).

test(pdif:dif, A) :-
    pif(pdif(a, b), A = true, A = false).
    
test(pdif:eq, A) :-
    pif(pdif(a, a), A = false, A = true).
 