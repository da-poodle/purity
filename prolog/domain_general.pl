% General domain
%
% a domain that applies to all elements and wraps compare/3.
%
:- module(domain_general, []).

:- multifile purity:domain/3.
:- multifile purity:pcompare/3.

purity:domain(general, Val, Val).

purity:pcompare(general(A), general(B), C) :-
    compare(C, A, B).