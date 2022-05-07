:- module(domain_unary, []).

:- use_module(punary).

:- multifile purity:domain/3.
:- multifile purity:pcompare/4.

purity:domain(punary, A, A) :- unary(A).

purity:pcompare(punary, A, B, C) :- unary_compare(A, B, C).

% unary_compare(Unary1, Unary2, Comparator).
%
% Comparator is the different type of Unary1 and Unary2 
% Comparator is one of =, <, or >
%
unary_compare(z, U2, C) :- unary_compare_z(U2, C).
unary_compare(c(Z), U2, C) :- unary_compare_c(U2, c(Z), C).

unary_compare_z(z, =).
unary_compare_z(c(_), <).

unary_compare_c(z, _, >).
unary_compare_c(c(Z2), c(Z1), C) :- 
	unary_compare(Z1, Z2, C).