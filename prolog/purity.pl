% Comparison Library
:- module(purity, [
    domain/3,
    domain/2,
    
    pcompare/3,
    pdif/2,
    pdif/3,
    
    pif/3, 
    (',')/3, 
    (;)/3,
    
    (=)/3,
    eq/2,
    eq/3,
    
    (<)/3,
    lt/2,
    lt/3,
    
    (<=)/3,
    lte/2,
    lte/3,

    (>)/3,
    gt/2,
    gt/3,

    (>=)/3,
    gte/2,
    gte/3    
]).

%:- reexport([pchar, plist, pstring, punary]).


% pcompare(TermA, TermB, Comparator) - 
%
% Comparator is one of <, > or =
% Comparator is the comparison between TermA and TermB
% both TermA and TermB must be in Domain
%
:- multifile(pcompare/3).

% domain(Domain, Value, DomainedValue).
%
% Holds if Value is the same as DomainedValue, except that DomainedValue 
% is specified as in Domain.
%
:- multifile(domain/3).

domain(X, Domained) :-
    domain(_, X, Domained).

:- meta_predicate(pif(1, 0, 0)).

% if(Goal, TrueGoal, FalseGoal).
%
% Goal is a callable with the last argument as a boolean (true or false).
% if Goal results in true then TrueGoal is called.
% if Goal results in false then FalseGoal is called.
%
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


pdif(A,B,R) :-
    pcompare(A,B,C),
    pdif_(C,R).

pdif_(=,false).
pdif_(<,true).
pdif_(>,true).

pdif(A,B) :- pdif(A, B, true).

% eq/2, eq/3, (=)/3
eq(A, B, T) :- compare_with_states(eq_, A, B, T).
eq(A, B) :-  eq(A, B, true).
=(A, B, T) :- eq(A, B, T).

eq_(=, true).
eq_(<, false).
eq_(>, false).

% lt/2, lt/3, (<)/3
lt(A, B, T) :- compare_with_states(lt_, A, B, T).
lt(A, B) :- lt(A, B, true).
<(A, B, T) :- lt(A, B, T).

lt_(=, false).
lt_(<, true).
lt_(>, false).

% lte/2, lte/3, (<=)/3
lte(A, B, T) :- compare_with_states(lte_, A, B, T).
lte(A, B) :- lte(A, B, true).
<=(A, B, T) :- lte(A, B, T).

lte_(=, true).
lte_(<, true).
lte_(>, false).

% gt/2, gt/3, (>)/3
gt(A, B, T) :- compare_with_states(gt_, A, B, T).
gt(A, B) :- gt(A, B, true).    
>(A, B, T) :- gt(A, B, T).

gt_(=, false).
gt_(<, false).
gt_(>, true).

% gte/2, gte/3, (>=)/3
gte(A, B, T) :- compare_with_states(gte_, A, B, T).
gte(A, B) :- gte(A, B, true).
>=(A, B, T) :- gte(A, B, T).

gte_(=, true).
gte_(<, false).
gte_(>, true).

compare_with_states(StateGoal, A, B, Truth) :-
    domain(A, DA),
    domain(B, DB),    
    pcompare(DA, DB, C),
    call(StateGoal, C, Truth).