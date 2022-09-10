% P03 - Find the K'th element of a list. 
% The first element in the list is number 1.
% Example:
%    element_at(X,[a,b,c,d,e],3).
% X = c

element_at_hc(C, [C|_], c(zero)).
element_at_hc(C, [_|T], c(c(N))) :-
    element_at_hc(C, T, c(N)).

element_at(Element, List, K) :-
    number_unary(K, U),
    element_at_hc(Element, List, U).

% allow conversion of human numbers to church encoded numbers up to 40
% for bigger numbers then use just unary or a different method can be used
numbers([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40]).

number_unary(N,U) :-
    numbers(Nums),
    number_unary(Nums, N, U).

number_unary([N|_],N,zero).
number_unary([_|T],N,c(Z)) :-
    number_unary(T,N,Z).
