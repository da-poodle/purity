% P04 - Find the number of elements of a list.
%
% Example:
%    n_elements([a,b,c,d], N)
% N = 4.
%
n_elements_hc([], zero).
n_elements_hc([_|T], c(N)) :-
    n_elements_hc(T, N).

n_elements(List, N) :-
    number_unary(N, U),
    n_elements_hc(List, U).

% allow conversion of human numbers to church encoded numbers up to 40
numbers([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40]).

number_unary(N,U) :-
    numbers(Nums),
    number_unary(Nums, N, U).

number_unary([N|_],N,zero).
number_unary([_|T],N,c(Z)) :-
    number_unary(T,N,Z).