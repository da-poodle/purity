:- module(punary, [
	unary/1,
    pow/3,
    string_unary/2,
    unary_string/2,
    add/3,
    mul/3,
    div/4
]).

:- use_module(purity).

% unary_compare(Unary1, Unary2, Comparator).
%
% Comparator is the different type of Unary1 and Unary2 
% Comparator is one of =, <, or >
%
unary_compare(zero, U2, C) :- unary_compare_z(U2, C).
unary_compare(c(Z), U2, C) :- unary_compare_c(U2, c(Z), C).

unary_compare_z(zero, =).
unary_compare_z(c(_), <).

unary_compare_c(zero, _, >).
unary_compare_c(c(Z2), c(Z1), C) :- 
	unary_compare(Z1, Z2, C).

% unary(Unary).
%
% Unary is a valid unary number
%
unary(zero).
unary(c(Z)) :- unary(Z).

% add(UnaryA, UnaryB, Sum).
%
% Sum is the value of UnaryA + UnaryB
%
add(zero, Z, Z).
add(c(X), Y, c(Z)) :- 
	add(X, Y, Z).

% mul(UnaryA, UnaryB, Product)
%
% Product is the result of UnaryA * UnaryB
%
mul(zero, _, zero).
mul(c(X), Y, R) :- 
	add(Y, S, R), 
	mul(X, Y, S).

% div(Divide, By, Quotient, Remainder)
%
% division with remainder
%
div(S, D, Q, R) :-
	unary_compare(zero, R, C),
	unary_div(C, S, D, Q, R).	

unary_div(=, S, D, Q, zero) :- 
	mul(D, Q, S).
unary_div(>, S, D, Q, zero) :- 
	mul(D, Q, S).
unary_div(<, S, D, Q, R) :- 	
	unary_compare(R, D, <),
	add(Is, R, S), 
	mul(D, Q, Is).

% upow(Unary, Factor, Power)
%
% Power is Unary^Factor
%
pow(zero, zero, zero).
pow(N, c(zero), N).
pow(N, c(c(Z)), R) :-
    mul(N, R1, R),
    pow(N, c(Z), R1).







%
% convert 0 - 9 into a unary value
%
d_to_u(U, N) :-
	d_to_u_(['0','1','2','3','4','5','6','7','8','9'], N, U).

d_to_u_([A|T], N, U) :-
	pdif(pchar, A, N, C),
	d_to_u_(C, N, T, U).

d_to_u_(false, _, _, zero).
d_to_u_(true, N, T, c(Z)) :-
	d_to_u_(T, N, Z).

%
% convert a unary value below 10 into 0 - 9
%
u_to_d(U, D) :-
	u_to_d_(U, ['0','1','2','3','4','5','6','7','8','9'], D).

u_to_d_(zero, [D|_], D).
u_to_d_(c(Z), [_|T], D) :-
	u_to_d_(Z, T, D).

% this is required for overlows, but is not a character that is used
unary_10( c(c(c(c(c(c(c(c(c(c(zero)))))))))) ).

%
% Convert strings to unary numbers
%
% NEEDS TESTING
%
u_dec_unary_([], _, Sum, Sum).
u_dec_unary_([D|T], Mult, Sum, R) :-	
	u_to_d(X, D),
	mul(Mult, X, Num),
	add(Sum, Num, Sum1),
	
	unary_10(M),	
	mul(Mult, M, NewMult),
	
	u_dec_unary_(T, NewMult, Sum1, R).
	
% string_unary(String, Unary).
%
% Unary is the punary version of String
%	
string_unary(D, C) :- 
	reverse(D, DR),
	u_dec_unary_(DR, c(zero), zero, C).

%
% convert unary numbers to strings
%
% NEEDS TESTING
%
u_unary_dec(zero, _, []).
u_unary_dec(c(Z), Mul, [V|T]) :-
	unary_10(U_10),
	mul(Mul, U_10, NewMul),
	div(c(Z), NewMul, LeftOver, R),
	u_to_d(R, V),
	
	u_unary_dec(LeftOver, NewMul, T).
	
% unary_string(Unary, String).
%
% String is the pstring version of Unary
%	
unary_string(U, S) :- 
	u_unary_dec(U, c(zero), R),
	reverse(R, S).
