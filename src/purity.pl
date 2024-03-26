:- multifile(pcompare/4).
:- multifile(ptype/2).
:- multifile(pcall/1).
:- multifile(pcall/2).

pcall(unify(A, B)) :- unify(A, B).
pcall((A , B)) :- A , B.

% unify(A, B). 
%
% Equivilent of A = B. 
%
unify(A, A).

% pcompare(A, B, C).
%
% The comparison of A and B is wither =, < or >.
%
pcompare(A, B, C) :-
    ptype(A, T),
    ptype(B, T),
    pcompare(T, A, B, C).

% pif(Goal, True, False).
%
% Goal is a term with the last argument of true or false.
% When true then TrueGoal is tested.
% When false then FalseGoal is tested.
%
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

% pdif(A, B, T)
%
% T is true if A and B are different.
% T is false if A and B are not different. 
% Note: This uses pcompare to determine difference.
%
pdif(A,B,T) :-
    pcompare(A,B,C),
    pdif_(C,T).

pdif_(=,false).
pdif_(<,true).
pdif_(>,true).

% pdif(A,B).
%
% Holds if A and B are determined to be different.
%
pdif(A,B) :- pdif(A, B, true).

% eq(A,B,T).
%
% T is true if A and B are equal, otherwise false.
%
eq(A, B, T) :- compare_with_states(eq_, A, B, T).

eq_(=, true).
eq_(<, false).
eq_(>, false).

% lt(A, B, T).
%
% T is true if A < B, otherwise false.
%
lt(A, B, T) :- compare_with_states(lt_, A, B, T).

lt_(=, false).
lt_(<, true).
lt_(>, false).

% lte(A,B,T).
%
% T is true if A <= B, otherwise false.
%
lte(A, B, T) :- compare_with_states(lte_, A, B, T).

lte_(=, true).
lte_(<, true).
lte_(>, false).

% gt(A,B,T).
%
% T is true if A > B, otherwise false.
%
gt(A, B, T) :- compare_with_states(gt_, A, B, T).

gt_(=, false).
gt_(<, false).
gt_(>, true).

% gte(A,B,T).
%
% T is true if A >= B, otherwise false.
%
gte(A, B, T) :- compare_with_states(gte_, A, B, T).

gte_(=, true).
gte_(<, false).
gte_(>, true).

compare_with_states(StateGoal, A, B, Truth) :-  
    pcompare(A, B, C),
    pcall(StateGoal, C, Truth).

pcall(pdif(A,B), T) :- pdif(A,B,T).
pcall(eq(A,B), T) :- eq(A,B,T).
pcall(lt(A,B), T) :- lt(A,B,T).
pcall(gt(A,B), T) :- gt(A,B,T).
pcall(lte(A, B), T) :- lte(A,B,T).
pcall(gte(A, B), T) :- gte(A,B,T).

pcall(pdif(A), B, T) :- pdif(A,B,T).
pcall(eq(A), B, T) :- eq(A,B,T).
pcall(lt(A), B, T) :- lt(A,B,T).
pcall(gt(A), B, T) :- gt(A,B,T).
pcall(lte(A), B, T) :- lte(A,B,T).
pcall(gte(A), B, T) :- gte(A,B,T).

pcall(eq_, C, T) :- eq_(C,T).
pcall(lt_, C, T) :- lt_(C,T).
pcall(gt_, C, T) :- gt_(C,T).
pcall(gte_, C, T) :- gte_(C,T).
pcall(lte_, C, T) :- lte_(C,T).
