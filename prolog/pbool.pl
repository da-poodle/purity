:- module(pbool, [
    pbool/1
]).

:- use_module(purity).

:- multifile purity:pcompare/4.

purity:pcompare(pbool, A, B, C) :- bool_compare(A, B, C).

% pbool(Bool)
%
% Bool is true or false
%
pbool(true).
pbool(false).

% bool_compare(BoolA, BoolB, Comparator).
%
% Comparator is either <, > or =
% true is considered 'higher' than false
%
bool_compare(true, B, C) :- compare_true(B, C).
bool_compare(false, B, C) :- compare_false(B, C).

compare_true(true, =).
compare_true(false, >).

compare_false(true, <).
compare_false(false, =).