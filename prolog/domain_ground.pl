% ground domain
%
% A domain that applies to all ground values.
%
:- module(domain_ground, []).

:- multifile purity:domain/3.
:- multifile purity:pcompare/3.

purity:domain(ground, Val, Val) :- ground(Val).

purity:pcompare(ground(A), ground(B), C) :-
    must_be(ground, A),
    must_be(ground, B),
    compare(C, A, B).


    