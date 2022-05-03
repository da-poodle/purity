:- module(seq_domain, []).

:- use_module(pseq).

:- multifile purity:domain/3.
:- multifile purity:pcompare/3.

purity:domain(seq, Val, seq(Val)) :-
    pseq(Val, _).

purity:pcompare(seq(A), seq(B), C) :-
    compare_seq(A,B,C).

compare_seq(A, A, =).
compare_seq(A, B, <) :- pseq:less(A,B).
compare_seq(A, B, >) :- pseq:less(B,A).