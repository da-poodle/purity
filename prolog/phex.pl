:- module(phex, []).

:- multifile(pcompare/3).
:- multifile(ptype/2).

% hex_compare(Hex1,Hex2,Comparison).
purity:pcompare(hex(A), hex(B), C) :-
    hex_map(A, B, C).

% hex2_compare(Hex1,Hex2,Comparison).
purity:pcompare(hex2(A1,B1), hex2(A2,B2), C) :-
    hex_map(A1, A2, Ch1),
    hex2_cmp(Ch1, B1, B2, C).

hex2_cmp(=, B1, B2, C) :- 
    hex_map(B1, B2, C).
hex2_cmp(>, _, _, >).
hex2_cmp(<, _, _, <).

purity:ptype(hex(H), hex) :-
    hex(H).
purity:ptype(hex2(H1,H2), hex2) :-
    hex(H1),
    hex(H2).

% hex(HexValue).
hex(0).
hex(1).
hex(2).
hex(3).
hex(4).
hex(5).
hex(6).
hex(7).
hex(8).
hex(9).
hex(a).
hex(b).
hex(c).
hex(d).
hex(e).
hex(f).

% map 0
hex_map(0, 0, =).
hex_map(0, 1, <).
hex_map(0, 2, <).
hex_map(0, 3, <).
hex_map(0, 4, <).
hex_map(0, 5, <).
hex_map(0, 6, <).
hex_map(0, 7, <).
hex_map(0, 8, <).
hex_map(0, 9, <).
hex_map(0, a, <).
hex_map(0, b, <).
hex_map(0, c, <).
hex_map(0, d, <).
hex_map(0, e, <).
hex_map(0, f, <).

% map 1
hex_map(1, 0, >).
hex_map(1, 1, =).
hex_map(1, 2, <).
hex_map(1, 3, <).
hex_map(1, 4, <).
hex_map(1, 5, <).
hex_map(1, 6, <).
hex_map(1, 7, <).
hex_map(1, 8, <).
hex_map(1, 9, <).
hex_map(1, a, <).
hex_map(1, b, <).
hex_map(1, c, <).
hex_map(1, d, <).
hex_map(1, e, <).
hex_map(1, f, <).

% map 2
hex_map(2, 0, >).
hex_map(2, 1, >).
hex_map(2, 2, =).
hex_map(2, 3, <).
hex_map(2, 4, <).
hex_map(2, 5, <).
hex_map(2, 6, <).
hex_map(2, 7, <).
hex_map(2, 8, <).
hex_map(2, 9, <).
hex_map(2, a, <).
hex_map(2, b, <).
hex_map(2, c, <).
hex_map(2, d, <).
hex_map(2, e, <).
hex_map(2, f, <).

% map 3
hex_map(3, 0, >).
hex_map(3, 1, >).
hex_map(3, 2, >).
hex_map(3, 3, =).
hex_map(3, 4, <).
hex_map(3, 5, <).
hex_map(3, 6, <).
hex_map(3, 7, <).
hex_map(3, 8, <).
hex_map(3, 9, <).
hex_map(3, a, <).
hex_map(3, b, <).
hex_map(3, c, <).
hex_map(3, d, <).
hex_map(3, e, <).
hex_map(3, f, <).

% map 4
hex_map(4, 0, >).
hex_map(4, 1, >).
hex_map(4, 2, >).
hex_map(4, 3, >).
hex_map(4, 4, =).
hex_map(4, 5, <).
hex_map(4, 6, <).
hex_map(4, 7, <).
hex_map(4, 8, <).
hex_map(4, 9, <).
hex_map(4, a, <).
hex_map(4, b, <).
hex_map(4, c, <).
hex_map(4, d, <).
hex_map(4, e, <).
hex_map(4, f, <).

% map 5
hex_map(5, 0, >).
hex_map(5, 1, >).
hex_map(5, 2, >).
hex_map(5, 3, >).
hex_map(5, 4, >).
hex_map(5, 5, =).
hex_map(5, 6, <).
hex_map(5, 7, <).
hex_map(5, 8, <).
hex_map(5, 9, <).
hex_map(5, a, <).
hex_map(5, b, <).
hex_map(5, c, <).
hex_map(5, d, <).
hex_map(5, e, <).
hex_map(5, f, <).

% map 6
hex_map(6, 0, >).
hex_map(6, 1, >).
hex_map(6, 2, >).
hex_map(6, 3, >).
hex_map(6, 4, >).
hex_map(6, 5, >).
hex_map(6, 6, =).
hex_map(6, 7, <).
hex_map(6, 8, <).
hex_map(6, 9, <).
hex_map(6, a, <).
hex_map(6, b, <).
hex_map(6, c, <).
hex_map(6, d, <).
hex_map(6, e, <).
hex_map(6, f, <).

% map 7
hex_map(7, 0, >).
hex_map(7, 1, >).
hex_map(7, 2, >).
hex_map(7, 3, >).
hex_map(7, 4, >).
hex_map(7, 5, >).
hex_map(7, 6, >).
hex_map(7, 7, =).
hex_map(7, 8, <).
hex_map(7, 9, <).
hex_map(7, a, <).
hex_map(7, b, <).
hex_map(7, c, <).
hex_map(7, d, <).
hex_map(7, e, <).
hex_map(7, f, <).

% map 8
hex_map(8, 0, >).
hex_map(8, 1, >).
hex_map(8, 2, >).
hex_map(8, 3, >).
hex_map(8, 4, >).
hex_map(8, 5, >).
hex_map(8, 6, >).
hex_map(8, 7, >).
hex_map(8, 8, =).
hex_map(8, 9, <).
hex_map(8, a, <).
hex_map(8, b, <).
hex_map(8, c, <).
hex_map(8, d, <).
hex_map(8, e, <).
hex_map(8, f, <).

% map 9
hex_map(9, 0, >).
hex_map(9, 1, >).
hex_map(9, 2, >).
hex_map(9, 3, >).
hex_map(9, 4, >).
hex_map(9, 5, >).
hex_map(9, 6, >).
hex_map(9, 7, >).
hex_map(9, 8, >).
hex_map(9, 9, =).
hex_map(9, a, <).
hex_map(9, b, <).
hex_map(9, c, <).
hex_map(9, d, <).
hex_map(9, e, <).
hex_map(9, f, <).

% map a
hex_map(a, 0, >).
hex_map(a, 1, >).
hex_map(a, 2, >).
hex_map(a, 3, >).
hex_map(a, 4, >).
hex_map(a, 5, >).
hex_map(a, 6, >).
hex_map(a, 7, >).
hex_map(a, 8, >).
hex_map(a, 9, >).
hex_map(a, a, =).
hex_map(a, b, >).
hex_map(a, c, <).
hex_map(a, d, <).
hex_map(a, e, <).
hex_map(a, f, <).

% map b
hex_map(b, 0, >).
hex_map(b, 1, >).
hex_map(b, 2, >).
hex_map(b, 3, >).
hex_map(b, 4, >).
hex_map(b, 5, >).
hex_map(b, 6, >).
hex_map(b, 7, >).
hex_map(b, 8, >).
hex_map(b, 9, >).
hex_map(b, a, >).
hex_map(b, b, =).
hex_map(b, c, <).
hex_map(b, d, <).
hex_map(b, e, <).
hex_map(b, f, <).

% map c
hex_map(c, 0, >).
hex_map(c, 1, >).
hex_map(c, 2, >).
hex_map(c, 3, >).
hex_map(c, 4, >).
hex_map(c, 5, >).
hex_map(c, 6, >).
hex_map(c, 7, >).
hex_map(c, 8, >).
hex_map(c, 9, >).
hex_map(c, a, >).
hex_map(c, b, >).
hex_map(c, c, =).
hex_map(c, d, <).
hex_map(c, e, <).
hex_map(c, f, <).

% map d
hex_map(d, 0, >).
hex_map(d, 1, >).
hex_map(d, 2, >).
hex_map(d, 3, >).
hex_map(d, 4, >).
hex_map(d, 5, >).
hex_map(d, 6, >).
hex_map(d, 7, >).
hex_map(d, 8, >).
hex_map(d, 9, >).
hex_map(d, a, >).
hex_map(d, b, >).
hex_map(d, c, >).
hex_map(d, d, =).
hex_map(d, e, <).
hex_map(d, f, <).

% map e
hex_map(e, 0, >).
hex_map(e, 1, >).
hex_map(e, 2, >).
hex_map(e, 3, >).
hex_map(e, 4, >).
hex_map(e, 5, >).
hex_map(e, 6, >).
hex_map(e, 7, >).
hex_map(e, 8, >).
hex_map(e, 9, >).
hex_map(e, a, >).
hex_map(e, b, >).
hex_map(e, c, >).
hex_map(e, d, >).
hex_map(e, e, =).
hex_map(e, f, <).

% map f
hex_map(f, 0, >).
hex_map(f, 1, >).
hex_map(f, 2, >).
hex_map(f, 3, >).
hex_map(f, 4, >).
hex_map(f, 5, >).
hex_map(f, 6, >).
hex_map(f, 7, >).
hex_map(f, 8, >).
hex_map(f, 9, >).
hex_map(f, a, >).
hex_map(f, b, >).
hex_map(f, c, >).
hex_map(f, d, >).
hex_map(f, e, >).
hex_map(f, f, =).
