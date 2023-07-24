:- module(purity, [
    pcompare/3,
    ptype/2,
    pif/3,
    pdif_t/3,
    pdif/2,
    ','/3,
    ';'/3,
    '='/3,
    '<'/3,
    '<='/3,
    '>'/3,
    '>='/3
]).

:- multifile(pcompare/4).
:- multifile(ptype/2).

:- meta_predicate(pif(1, 0, 0)).

pcompare(A, B, C) :-
    ptype(A, T),
    ptype(B, T),
    pcompare(T, A, B, C).

pif(Goal, TrueGoal, FalseGoal) :-
    call(Goal, R),
    pif_(R, TrueGoal, FalseGoal).

pif_(true, Goal, _) :- call(Goal).
pif_(false, _, Goal) :- call(Goal).

','(A, B, R) :-
    call(A, T),
    conj_(T, B, R).

conj_(true, B, R) :-
    call(B, R).
conj_(false, _, false).

';'(A, B, T) :-
    call(A, T)
    ; 
    call(B, T).

pdif_t(A,B,R) :-
    pcompare(A,B,C),
    pdif_(C,R).

pdif_(=,false).
pdif_(<,true).
pdif_(>,true).

pdif(A,B) :- pdif_t(A, B, true).

=(A, B, T) :- compare_with_states(eq_, A, B, T).

eq_(=, true).
eq_(<, false).
eq_(>, false).

<(A, B, T) :- compare_with_states(lt_, A, B, T).

lt_(=, false).
lt_(<, true).
lt_(>, false).

<=(A, B, T) :- compare_with_states(lte_, A, B, T).

lte_(=, true).
lte_(<, true).
lte_(>, false).

>(A, B, T) :- compare_with_states(gt_, A, B, T).

gt_(=, false).
gt_(<, false).
gt_(>, true).

>=(A, B, T) :- compare_with_states(gte_, A, B, T).

gte_(=, true).
gte_(<, false).
gte_(>, true).

compare_with_states(StateGoal, A, B, Truth) :-  
    pcompare(A, B, C),
    call(StateGoal, C, Truth).

