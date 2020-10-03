% Comparison Library
:- module(purity, [
    pcompare/4,
    pdif/3,
    pdif/4,
    pif/3,
    eq/3,
    eq/4,
    lt/3,
    lt/4,
    lte/3,
    lte/4,
    gt/3,
    gt/4,    
    gte/3,
    gte/4    
]).

% pcompare(Domain, TermA, TermB, Comparator) - 
%
% Comparator is one of <, > or =
% Comparator is the comparison between TermA and TermB
% both TermA and TermB must be in Domain
%
:- multifile(pcompare/4).

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


% pdif(Domain, TermA, TermB, Result).
%
% Result is true if TermA is not TermB for domain, otherwise false
%
pdif(D,A,B,R) :-
    pcompare(D,A,B,C),
    pdif_(C,R).

pdif_(=,false).
pdif_(<,true).
pdif_(>,true).

% pdif(Domain, TermA, TermB).
% 
% TermA is not TermB 
% TermA and TermB must be in Domain
% 
pdif(D,A,B) :- pdif(D, A, B, true).


% eq(Domain, TermA, TermB).
%
% TermA and TermB are equal for Domain.
%
eq(D, A, B) :- pcompare(D, A, B, =).

% eq(Domain, TermA, TermB, Result).
%
% Result is true if TermA and TermB are equal for Domain.
% Result is false if TermA and TermB are equal for Domain.
%
eq(D, A, B, T) :- pcompare(D, A, B, C), eq_(C, T).

eq_(=, true).
eq_(<, false).
eq_(>, false).


% lt(Domain, TermA, TermB). 
%
% TermA is less than TermB for Domain.
%
lt(D, A, B) :- pcompare(D, A, B, <).

% lt(Domain, TermA, TermB, Result).
%
% Result is true if TermA is less than TermB for Domain.
% Result is false if TermA is not less than TermB for Domain.
%
lt(D, A, B, T) :- pcompare(D, A, B, C), lt_(C, T).

lt_(=, false).
lt_(<, true).
lt_(>, false).


% lte(Domain, TermA, TermB). 
%
% TermA is less than or equal to TermB for Domain.
%
lte(D, A, B) :- pcompare(D, A, B, C), lte_(C, true).

% lte(Domain, TermA, TermB, Result).
%
% Result is true if TermA is less than or equal to TermB for Domain.
% Result is false if TermA is not less than or equal to TermB for Domain.
%
lte(D, A, B, T) :- pcompare(D, A, B, C), lte_(C, T).

lte_(=, true).
lte_(<, true).
lte_(>, false).


% gt(Domain, TermA, TermB). 
%
% TermA is greater than TermB for Domain
%
gt(D, A, B) :- pcompare(D, A, B, >).

% gt(Domain, TermA, TermB, Result).
%
% Result is true if TermA is greater than TermB for Domain.
% Result is false if TermA is greater than TermB for Domain.
%
gt(D, A, B, T) :- pcompare(D, A, B, C), gt_(C, T).

gt_(=, false).
gt_(<, false).
gt_(>, true).


% gte(Domain, TermA, TermB). 
% 
% TermA is greater than or equal to TermB for domain.
%
gte(D, A, B) :- pcompare(D, A, B, C), gte_(C, true).

% gte(Domain, TermA, TermB, Result).
%
% Result is true if TermA is greater than or equal to TermB for Domain.
% Result is false if TermA is greater than or equal to TermB for Domain.
%
gte(D, A, B, T) :- pcompare(D, A, B, C), gte_(C, T).

gte_(=, true).
gte_(<, false).
gte_(>, true).
