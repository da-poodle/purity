% plist domain (plist)
%
% A domain that works over lists
%
:- module(domain_plist, []).

:- use_module(plist).

:- multifile purity:domain/3.
:- multifile purity:pcompare/3.

purity:domain(plist, A, B) :- 
    plist(A),
    maplist(domain(_), A, B).
purity:domain(plist(Domain), A, B) :-
    plist(A),
    maplist(domain(Domain), A, B).

purity:pcompare(A, B, C) :- 
    domain(plist(D), _, A),
    domain(plist(D), _, B),
    plist_compare(A, B, C, D). 

