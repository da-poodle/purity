:- module(pdomains, []).

:- multifile(purity:pcompare/3).

purity:pcompare(general(A), general(B), C) :-
    compare(C, A, B).

purity:pcompare(ground(A), ground(B), C) :-
    must_be(ground, A),
    must_be(ground, B),
    compare(C, A, B).


    