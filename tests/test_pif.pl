:- use_module('../prolog/purity.pl').

purity:pcompare(A, B, C) :- pchar_compare(A, B, C).

% 99 prolog problems 
% taken from:  https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

% P01 (*) Find the last element of a list. 
p1_last([A],A).
p1_last([_|T],A) :-
    p1_last(T,A).

% P02 (*) Find the last but one element of a list. 
p2_last_but_one([E,_], E).
p2_last_but_one([_,B,C|T], E) :- p2_last_but_one([B,C|T], E).

% P03 (*) Find the K'th element of a list. 
% The first element in the list is number 1.
% Example:
% ?- element_at(X,[a,b,c,d,e],3).
% X = c
element_at(C, [C|_], c(zero)).
element_at(C, [_|T], c(c(N))) :-
    element_at(C, T, c(N)).

% P04 (*) Find the number of elements of a list.
n_elements([], zero).
n_elements([_|T], c(N)) :-
    n_elements(T, N).

% P05 (*) Reverse a list.
% solution taken from https://courses.cs.washington.edu/courses/cse341/10wi/clpr/difference_lists.clpr
preverse(Xs,Rs) :- reverse_dl(Xs,Rs-[]).

reverse_dl([],T-T).
reverse_dl([X|Xs],Rs-T) :- reverse_dl(Xs,Rs-[X|T]).

% P06 (*) Find out whether a list is a palindrome.
%    A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
palindrome(N) :-
    preverse(N, N).

% P07 (**) Flatten a nested list structure.
%     Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
%
%     Example:
%     ?- my_flatten([a, [b, [c, d], e]], X).
%     X = [a, b, c, d, e]
%
%     Hint: Use the predefined predicates is_list/1 and append/3
my_flatten(S,F) :-
  flatten_dl(S, F-[]).

flatten_dl([], X-X).
flatten_dl([X|Xs], Y-Z) :-
   flatten_dl(X, Y-T),
   flatten_dl(Xs, T-Z).
flatten_dl(A, [A|Z]-Z) :- ptype(A, domain).

% P08 (**) Eliminate consecutive duplicates of list elements.
%    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
%
%    Example:
%    ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [a,b,c,a,d,e]
compress([], []).
compress([A|T], [Dedupped|Result]) :-
    remove_seq([A|T], Dedupped, Remaining),
    compress(Remaining, Result).

remove_seq([A], A, []).
remove_seq([A,A|T], A, Remaining) :-
    remove_seq([A|T], _, Remaining).
remove_seq([A,B|T], A, [B|T]) :-
    pdif(A,B).

% P09 (**) Pack consecutive duplicates of list elements into sublists.
%    If a list contains repeated elements they should be placed in separate sublists.
%
%    Example:
%    ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
pack([], []).
pack([A|T], [Dedupped|Result]) :-
    pack_similar([A|T], Dedupped, Remaining),
    pack(Remaining, Result).

pack_similar([A], [A], []).
pack_similar([A,A|T], [A|Similar], Remaining) :-
    pack_similar([A|T], Similar, Remaining).
pack_similar([A,B|T], [A], [B|T]) :-
    pdif(A,B).

% P10 (*) Run-length encoding of a list.
%    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.
%
%    Example:
%    ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]    
encode(List, Encoded) :-
    pack(List, Packed),
    maplist(encoded_counted, Packed, Encoded).

encoded_counted(Packed, [N, A]) :-
    encoded_counted(Packed, A, N).

encoded_counted([A], A, c(zero)).
encoded_counted([A|T], A, c(c(N))) :-
    encoded_counted(T, A, c(N)).

% P11 (*) Modified run-length encoding.
%    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as [N,E] terms.
%
%    Example:
%    ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_modified(List, Encoded) :-
    pack(List, Packed),
    maplist(encoded_counted_mod, Packed, Encoded).

encoded_counted_mod(Packed, R) :-
    encoded_counted_mod(Packed, D, N),
    encode_by_type(N, D, R).

encode_by_type(c(zero), D, D).
encode_by_type(c(c(N)), D, [c(c(N)), D]).

encoded_counted_mod([A], A, c(zero)).
encoded_counted_mod([A|T], A, c(c(N))) :-
    encoded_counted_mod(T, A, c(N)).


% P12 (**) Decode a run-length encoded list.
%    Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
decode([], []).
decode([X|T], [X|D]) :- 
    ptype(X, domain),
    decode(T, D).
decode([[c(zero),X]|T], [X|D]) :- 
    decode(T,D).
decode([[c(c(N)),X]|T], [X|D]) :- 
    decode([[c(N),X]|T], D).


% P13 (**) Run-length encoding of a list (direct solution).
%    Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton terms [1,X] by X.
%
%    Example:
%    ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_direct([],[]).
encode_direct([X|Xs],[Z|Zs]) :- count(X,Xs,Ys,c(zero),Z), encode_direct(Ys,Zs).

% count(X,Xs,Ys,K,T) Ys is the list that remains from the list Xs
%    when all leading copies of X are removed. T is the term [N,X],
%    where N is K plus the number of X's that can be removed from Xs.
%    In the case of N=1, T is X, instead of the term [1,X].

count(X,[],[],c(zero),X).
count(X,[],[],c(c(N)),[c(c(N)),X]). 
count(X,[Y|Ys],[Y|Ys],c(zero),X) :- pdif(X, Y).
count(X,[Y|Ys],[Y|Ys],c(c(N)),[c(c(N)),X]) :- pdif(X, Y).
count(X,[X|Xs],Ys,N,T) :- count(X,Xs,Ys,c(N),T).


% P14 (*) Duplicate the elements of a list.
%    Example:
%    ?- dupli([a,b,c,c,d],X).
%    X = [a,a,b,b,c,c,c,c,d,d]
dupli([],[]).
dupli([X|T],[X,X|R]) :- dupli(T,R).

% P15 (**) Duplicate the elements of a list a given number of times.
%    Example:
%    ?- dupli([a,b,c],3,X).
%    X = [a,a,a,b,b,b,c,c,c]
%
%    What are the results of the goal:
%    ?- dupli(X,3,Y).
dupli([],_,[]).
dupli([X|Xs],N,Ys) :- duplix(N,X,Ys-T), dupli(Xs,N,T).

duplix(zero,_,T-T).
duplix(c(N),X,[X|R]-T) :-
    duplix(N,X,R-T).

% P16 (**) Drop every N'th element from a list.
%    Example:
%    ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
%    X = [a,b,d,e,g,h,k]
drop(Xs,N,Ys) :- drop(Xs,N,N,Ys).

drop([],_,_,[]).
drop([_|Xs],N,c(zero),Ys) :- 
    drop(Xs,N,N,Ys).
drop([X|Xs],N,c(c(Nc)),[X|Ys]) :- 
    drop(Xs,N,c(Nc),Ys).

% P17 (*) Split a list into two parts; the length of the first part is given.
%    Do not use any predefined predicates.
%
%    Example:
%    ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
%    L1 = [a,b,c]
%    L2 = [d,e,f,g,h,i,k]
split(L,zero,[],L).
split([X|Xs],c(N),[X|Ys],L2) :- 
    split(Xs,N,Ys,L2).


% P18 (**) Extract a slice from a list.
%    Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
%
%    Example:
%    ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
%    X = [c,d,e,f,g]
slice([X|Xs],c(zero),c(End),[X|Ys]) :- 
    slice(Xs,End,Ys).
slice([_|Xs],c(Start),c(End),Ys) :- 
    slice(Xs,Start,End,Ys).

slice([X|_],c(zero),[X]).
slice([X|Xs],c(End),[X|Ys]) :-  
    slice(Xs,End,Ys).

% P19 (**) Rotate a list N places to the left.
%    Examples:
%    ?- rotate([a,b,c,d,e,f,g,h],3,X).
%    X = [d,e,f,g,h,a,b,c]
%
%    ?- rotate([a,b,c,d,e,f,g,h],-2,X).
%    X = [g,h,a,b,c,d,e,f]
%
%    Hint: Use the predefined predicates length/2 and append/3, as well as the result of problem P17.
rotate(left(N), List, Result) :-
    split(List, N, L1, L2),
    append(L2, L1, Result).
rotate(right(N), List, Result) :-
    plength(List, Len),
    add(NDiff, N, Len) ,
    rotate(left(c(NDiff)), List, Result).

% P20 (*) Remove the K'th element from a list.
% Example:
% ?- remove_at(X,[a,b,c,d],2,R).
% X = b
% R = [a,c,d]
remove_at_(c(zero), [X|T], X, T).
remove_at_(c(c(N)), [A|T], X, [A|R]) :-
    remove_at_(c(N), T, X, R).

remove_at(X, List, N, R) :-
    remove_at_(N, List, X, R).

% P21 (*) Insert an element at a given position into a list.
% Example:
% ?- insert_at(alfa,[a,b,c,d],2,L).
% L = [a,alfa,b,c,d]
insert_at(E, List, Pos, Result) :-
    remove_at_(Pos, Result, E, List).

% P22 (*) Create a list containing all integers within a given range.
% Example:
% ?- range(4,9,L).
% L = [4,5,6,7,8,9]
range(c(N), c(N), [c(N)]).
range(N, N1, [N|T]) :-
    unary_compare(N, N1, <),
    range(c(N), N1, T).


/* skipping a few exercises until I find a way to do randomness */


% P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
% In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities (via backtracking).
%
% Example:
% ?- combination(3,[a,b,c,d,e,f],L).
% L = [a,b,c] ;
% L = [a,b,d] ;
% L = [a,b,e] ;
% ...
combination(N, L, R) :-
    plength(R, N),
    select_all(R, L).

select_all([], _).
select_all([A|T], L) :-
    el(A,L,Next),
    select_all(T,Next).

el(X,[X|L],L).
el(X,[_|L],R) :- el(X,L,R).