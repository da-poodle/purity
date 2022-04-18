% Comparison Library
:- module(purity, [
    domain/3,
    pcompare/3,
    pdif/2,
    pdif/3,
    pif/3,
    eq/2,
    eq/3,
    lt/2,
    lt/3,
    lte/2,
    lte/3,
    gt/2,
    gt/3,    
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

list_domain_( [], [], _ ).
list_domain_( [Value|T], [DValue|Rest], Domain ) :- 
    domain( Domain, Value, DValue ),
	list_domain_( T, Rest, Domain ).

domain( list(Domain), List, DomainedList ) :-
    list_domain_( List, DomainedList, Domain ).


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


% pdif(TermA, TermB, Result).
%
% Result is true if TermA is not TermB for otherwise false
%
pdif(A,B,R) :-
    pcompare(A,B,C),
    pdif_(C,R).

pdif_(=,false).
pdif_(<,true).
pdif_(>,true).

% pdif(TermA, TermB).
% 
% TermA is not TermB 
% TermA and TermB must be in Domain
% 
pdif(A,B) :- pdif(A, B, true).


% eq(TermA, TermB).
%
% TermA and TermB are equal for Domain.
%
eq(A, B) :- pcompare(A, B, =).

% eq(TermA, TermB, Result).
%
% Result is true if TermA and TermB are equal for Domain.
% Result is false if TermA and TermB are equal for Domain.
%
eq(A, B, T) :- 
    pcompare(A, B, C), 
    eq_(C, T).

eq_(=, true).
eq_(<, false).
eq_(>, false).


% lt(TermA, TermB). 
%
% TermA is less than TermB for Domain.
%
lt(A, B) :- pcompare(A, B, <).

% lt(TermA, TermB, Result).
%
% Result is true if TermA is less than TermB for Domain.
% Result is false if TermA is not less than TermB for Domain.
%
lt(A, B, T) :- 
    pcompare(A, B, C), 
    lt_(C, T).

lt_(=, false).
lt_(<, true).
lt_(>, false).


% lte(TermA, TermB). 
%
% TermA is less than or equal to TermB for Domain.
%
lte(A, B) :- 
    pcompare(A, B, C), 
    lte_(C, true).

% lte(TermA, TermB, Result).
%
% Result is true if TermA is less than or equal to TermB for Domain.
% Result is false if TermA is not less than or equal to TermB for Domain.
%
lte(A, B, T) :- 
    pcompare(A, B, C), 
    lte_(C, T).

lte_(=, true).
lte_(<, true).
lte_(>, false).


% gt(TermA, TermB). 
%
% TermA is greater than TermB for Domain
%
gt(A, B) :- pcompare(A, B, >).

% gt(TermA, TermB, Result).
%
% Result is true if TermA is greater than TermB for Domain.
% Result is false if TermA is greater than TermB for Domain.
%
gt(A, B, T) :- 
    pcompare(A, B, C), 
    gt_(C, T).

gt_(=, false).
gt_(<, false).
gt_(>, true).


% gte(TermA, TermB). 
% 
% TermA is greater than or equal to TermB for domain.
%
gte(A, B) :- 
    pcompare(A, B, C), 
    gte_(C, true).

% gte(TermA, TermB, Result).
%
% Result is true if TermA is greater than or equal to TermB for Domain.
% Result is false if TermA is greater than or equal to TermB for Domain.
%
gte(A, B, T) :- 
    pcompare(A, B, C), 
    gte_(C, T).

gte_(=, true).
gte_(<, false).
gte_(>, true).
