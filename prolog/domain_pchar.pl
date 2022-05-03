/*
    pchar domain (pchar)

    A domain that uses the pchar set of characters
*/
:- module(domain_pchar, []).

:- multifile purity:domain/3.
:- multifile purity:pcompare/3.

:- use_module(pchar).

purity:domain(pchar, Val, pchar(Val)) :-
    pchar_type(Val, _).

purity:pcompare(pchar(A), pchar(B), C) :-
    pchar_hex2(A, Ha),
    pchar_hex2(B, Hb),
    purity:pcompare(Ha, Hb, C).
