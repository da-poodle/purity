:- multifile(pcompare/4).
:- multifile(ptype/2).
:- multifile(pcall/1).
:- multifile(pcall/2).

pcall((A = B)) :- A = B.
pcall((A , B)) :- A , B.
pcall((A ; B)) :- A ; B.

pcompare(A, B, C) :-
    ptype(A, T),
    ptype(B, T),
    pcompare(T, A, B, C).

pif(Goal, TrueGoal, FalseGoal) :-
    pcall(Goal, R),
    pif_(R, TrueGoal, FalseGoal).

pif_(true, Goal, _) :- pcall(Goal).
pif_(false, _, Goal) :- pcall(Goal).

','(A, B, R) :-
    pcall(A, T),
    conj_(T, B, R).

conj_(true, B, R) :-
    pcall(B, R).
conj_(false, _, false).

';'(A, B, T) :-
    pcall(A, T)
    ; 
    pcall(B, T).

pdif(A,B,R) :-
    pcompare(A,B,C),
    pdif_(C,R).

pdif_(=,false).
pdif_(<,true).
pdif_(>,true).

pdif(A,B) :- pdif(A, B, true).

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
    pcall(StateGoal, C, Truth).

pcall(pdif(A,B), T) :- pdif(A,B,T).
pcall((A = B), T) :- '='(A,B,T).
pcall((A < B), T) :- '<'(A,B,T).
pcall((A > B), T) :- '>'(A,B,T).
pcall('<='(A, B), T) :- '<='(A,B,T).
pcall('>='(A, B), T) :- '>='(A,B,T).

pcall(eq_, C, T) :- eq_(C,T).
pcall(lt_, C, T) :- lt_(C,T).
pcall(gt_, C, T) :- gt_(C,T).
pcall(gte_, C, T) :- gte_(C,T).
pcall(lte_, C, T) :- lte_(C,T).

