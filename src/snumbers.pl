% Reverse a list
preverse(Xs, Ys) :-
    same_length(Xs, Ys),
    preverse(Xs, [], Ys, Ys).

preverse([], Ys, Ys, []).
preverse([X|Xs], Rs, Ys, [_|Bound]) :-
    preverse(Xs, [X|Rs], Ys, Bound).

% Base case: The sum of an empty list is the empty list.
sn_add([A|At], [B|Bt], [R|Rt]) :-
    preverse([A|At], Ar),
    preverse([B|Bt], Br),
    sn_add_(Ar, Br, Rr, '0'),
    preverse(Rr, [R|Rt]).
   
% end of list, no carry
sn_add_([], [], [], '0').
% end of list, with carry
sn_add_([], [], [C], C) :- 
    nonzero(C).
% A list empty
sn_add_([], [A|T], [B|R], C) :- 
    sn_sum(A,C,B,C2), 
    sn_add_([], T, R, C2).
% B list empty
sn_add_([A|T], [], [B|R], C) :- 
    sn_sum(A,C,B,C2), 
    sn_add_(T, [], R, C2).
% neither list empty, no carry
sn_add_([A|At], [B|Bt], [R|Rt], '0') :- 
    sn_sum(A, B, R, C),
    sn_add_(At, Bt, Rt, C).
% neither list empty, with carry
sn_add_([A|At], [B|Bt], [R|Rt], N) :- 
    nonzero(N),
    sn_sum(A, B, R0, C1),
    sn_sum(R0, N, R, C2), % problem is carry is too high?
    sn_add_carry(C1, C2, Cr, '0'),
    sn_add_(At, Bt, Rt, Cr).

sn_add_carry(A, '0', A, '0').
sn_add_carry(A, N, R, C) :- nonzero(N), sn_sum(A, N, R, C). 

nonzero('1').
nonzero('2').
nonzero('3').
nonzero('4').
nonzero('5').
nonzero('6').
nonzero('7').
nonzero('8').
nonzero('9').


% Addition tables
% sn_sum(A, B, Result, Carry).
%
sn_sum('0','0','0','0').
sn_sum('0','1','1','0').
sn_sum('0','2','2','0').
sn_sum('0','3','3','0').
sn_sum('0','4','4','0').
sn_sum('0','5','5','0').
sn_sum('0','6','6','0').
sn_sum('0','7','7','0').
sn_sum('0','8','8','0').
sn_sum('0','9','9','0').

sn_sum('1','0','1','0').
sn_sum('1','1','2','0').
sn_sum('1','2','3','0').
sn_sum('1','3','4','0').
sn_sum('1','4','5','0').
sn_sum('1','5','6','0').
sn_sum('1','6','7','0').
sn_sum('1','7','8','0').
sn_sum('1','8','9','0').
sn_sum('1','9','0','1').

sn_sum('2','0','2','0').
sn_sum('2','1','3','0').
sn_sum('2','2','4','0').
sn_sum('2','3','5','0').
sn_sum('2','4','6','0').
sn_sum('2','5','7','0').
sn_sum('2','6','8','0').
sn_sum('2','7','9','0').
sn_sum('2','8','0','1').
sn_sum('2','9','1','1').

sn_sum('3','0','3','0').
sn_sum('3','1','4','0').
sn_sum('3','2','5','0').
sn_sum('3','3','6','0').
sn_sum('3','4','7','0').
sn_sum('3','5','8','0').
sn_sum('3','6','9','0').
sn_sum('3','7','0','1').
sn_sum('3','8','1','1').
sn_sum('3','9','2','1').

sn_sum('4','0','4','0').
sn_sum('4','1','5','0').
sn_sum('4','2','6','0').
sn_sum('4','3','7','0').
sn_sum('4','4','8','0').
sn_sum('4','5','9','0').
sn_sum('4','6','0','1').
sn_sum('4','7','1','1').
sn_sum('4','8','2','1').
sn_sum('4','9','3','1').

sn_sum('5','0','5','0').
sn_sum('5','1','6','0').
sn_sum('5','2','7','0').
sn_sum('5','3','8','0').
sn_sum('5','4','9','0').
sn_sum('5','5','0','1').
sn_sum('5','6','1','1').
sn_sum('5','7','2','1').
sn_sum('5','8','3','1').
sn_sum('5','9','4','1').

sn_sum('6','0','6','0').
sn_sum('6','1','7','0').
sn_sum('6','2','8','0').
sn_sum('6','3','9','0').
sn_sum('6','4','0','1').
sn_sum('6','5','1','1').
sn_sum('6','6','2','1').
sn_sum('6','7','3','1').
sn_sum('6','8','4','1').
sn_sum('6','9','5','1').

sn_sum('7','0','7','0').
sn_sum('7','1','8','0').
sn_sum('7','2','9','0').
sn_sum('7','3','0','1').
sn_sum('7','4','1','1').
sn_sum('7','5','2','1').
sn_sum('7','6','3','1').
sn_sum('7','7','4','1').
sn_sum('7','8','5','1').
sn_sum('7','9','6','1').

sn_sum('8','0','8','0').
sn_sum('8','1','9','0').
sn_sum('8','2','0','1').
sn_sum('8','3','1','1').
sn_sum('8','4','2','1').
sn_sum('8','5','3','1').
sn_sum('8','6','4','1').
sn_sum('8','7','5','1').
sn_sum('8','8','6','1').
sn_sum('8','9','7','1').

sn_sum('9','0','9','0').
sn_sum('9','1','0','1').
sn_sum('9','2','1','1').
sn_sum('9','3','2','1').
sn_sum('9','4','3','1').
sn_sum('9','5','4','1').
sn_sum('9','6','5','1').
sn_sum('9','7','6','1').
sn_sum('9','8','7','1').
sn_sum('9','9','8','1').

% times tables
sn_product('0','0',['0']).
sn_product('0','1',['0']).
sn_product('0','2',['0']).
sn_product('0','3',['0']).
sn_product('0','4',['0']).
sn_product('0','5',['0']).
sn_product('0','6',['0']).
sn_product('0','7',['0']).
sn_product('0','8',['0']).
sn_product('0','9',['0']).

sn_product('1','0',['0']).
sn_product('1','1',['1']).
sn_product('1','2',['2']).
sn_product('1','3',['3']).
sn_product('1','4',['4']).
sn_product('1','5',['5']).
sn_product('1','6',['6']).
sn_product('1','7',['7']).
sn_product('1','8',['8']).
sn_product('1','9',['9']).

sn_product('2','0',['0']).
sn_product('2','1',['2']).
sn_product('2','2',['4']).
sn_product('2','3',['6']).
sn_product('2','4',['8']).
sn_product('2','5',['1','0']).
sn_product('2','6',['1','2']).
sn_product('2','7',['1','4']).
sn_product('2','8',['1','6']).
sn_product('2','9',['1','8']).

sn_product('3','0',['0']).
sn_product('3','1',['3']).
sn_product('3','2',['6']).
sn_product('3','3',['9']).
sn_product('3','4',['1','2']).
sn_product('3','5',['1','5']).
sn_product('3','6',['1','8']).
sn_product('3','7',['2','1']).
sn_product('3','8',['2','4']).
sn_product('3','9',['2','7']).

sn_product('4','0',['0']).
sn_product('4','1',['4']).
sn_product('4','2',['8']).
sn_product('4','3',['1','2']).
sn_product('4','4',['1','6']).
sn_product('4','5',['2','0']).
sn_product('4','6',['2','4']).
sn_product('4','7',['2','8']).
sn_product('4','8',['3','2']).
sn_product('4','9',['3','6']).

sn_product('5','0',['0']).
sn_product('5','1',['5']).
sn_product('5','2',['1','0']).
sn_product('5','3',['1','5']).
sn_product('5','4',['2','0']).
sn_product('5','5',['2','5']).
sn_product('5','6',['3','0']).
sn_product('5','7',['3','5']).
sn_product('5','8',['4','0']).
sn_product('5','9',['4','5']).

sn_product('6','0',['0']).
sn_product('6','1',['6']).
sn_product('6','2',['1','2']).
sn_product('6','3',['1','8']).
sn_product('6','4',['2','4']).
sn_product('6','5',['3','0']).
sn_product('6','6',['3','6']).
sn_product('6','7',['4','2']).
sn_product('6','8',['4','8']).
sn_product('6','9',['5','4']).

sn_product('7','0',['0']).
sn_product('7','1',['7']).
sn_product('7','2',['1','4']).
sn_product('7','3',['2','1']).
sn_product('7','4',['2','8']).
sn_product('7','5',['3','5']).
sn_product('7','6',['4','2']).
sn_product('7','7',['4','9']).
sn_product('7','8',['5','6']).
sn_product('7','9',['6','3']).

sn_product('8','0',['0']).
sn_product('8','1',['8']).
sn_product('8','2',['1','6']).
sn_product('8','3',['2','4']).
sn_product('8','4',['3','2']).
sn_product('8','5',['4','0']).
sn_product('8','6',['4','8']).
sn_product('8','7',['5','6']).
sn_product('8','8',['6','4']).
sn_product('8','9',['7','2']).

sn_product('9','0',['0']).
sn_product('9','1',['9']).
sn_product('9','2',['1','8']).
sn_product('9','3',['2','7']).
sn_product('9','4',['3','6']).
sn_product('9','5',['4','5']).
sn_product('9','6',['5','4']).
sn_product('9','7',['6','3']).
sn_product('9','8',['7','2']).
sn_product('9','9',['8','1']).
