:- use_module('../prolog/purity.pl').
:- use_module('../prolog/plist.pl').
:- use_module('../prolog/pseq.pl').
:- use_module('../prolog/pchar.pl').

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
element_at(C, [C|_], 1).
element_at(C, [_|T], N) :-
    pseq(N0, N),
    element_at(C, T, N0).

% P04 (*) Find the number of elements of a list.
n_elements([], 0).
n_elements([_|T], N) :-
    n_elements(T, N0),
    pseq(N0, N).

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

encoded_counted([A], A, 1).
encoded_counted([A|T], A, N) :-
    pseq(N0, N),
    encoded_counted(T, A, N0).

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
    pif(pseq_one_t(N), R = D, R = [N, D]).

encoded_counted_mod([A], A, 1).
encoded_counted_mod([A|T], A, N) :-
    pseq(N0, N),
    encoded_counted_mod(T, A, N0).


% P12 (**) Decode a run-length encoded list.
%    Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
decode([], []).
decode([X|T], [X|D]) :- 
    ptype(X, domain), 
    decode(T, D).
decode([[1,X]|T], [X|D]) :- 
    decode(T,D).
decode([[N,X]|T], [X|D]) :- 
    pseq_other_t(N, true), 
    pseq(N0,N), 
    decode([[N0,X]|T], D).


% P13 (**) Run-length encoding of a list (direct solution).
%    Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton terms [1,X] by X.
%
%    Example:
%    ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_direct([],[]).
encode_direct([X|Xs],[Z|Zs]) :- count(X,Xs,Ys,1,Z), encode_direct(Ys,Zs).

% count(X,Xs,Ys,K,T) Ys is the list that remains from the list Xs
%    when all leading copies of X are removed. T is the term [N,X],
%    where N is K plus the number of X's that can be removed from Xs.
%    In the case of N=1, T is X, instead of the term [1,X].

count(X,[],[],1,X).
count(X,[],[],N,[N,X]) :- pseq_other_t(N, true). 
count(X,[Y|Ys],[Y|Ys],1,X) :- pdif(X, Y).
count(X,[Y|Ys],[Y|Ys],N,[N,X]) :- pseq_other_t(N, true), pdif(X, Y).
count(X,[X|Xs],Ys,K,T) :- pseq(K, K1), count(X,Xs,Ys,K1,T).


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
dupli([X|Xs],N,Ys) :- duplix(X,N,Ys-T), dupli(Xs,N,T).

duplix(_,0,T-T).
duplix(X,N,[X|R]-T) :-
    pseq(N0,N),
    duplix(X,N0,R-T).

% P16 (**) Drop every N'th element from a list.
%    Example:
%    ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
%    X = [a,b,d,e,g,h,k]
drop(Xs,N,Ys) :- drop(Xs,N,N,Ys).

drop([],_,_,[]).
drop([_|Xs],N,1,Ys) :- 
    drop(Xs,N,N,Ys).
drop([X|Xs],N,Nc,[X|Ys]) :- 
    pseq_other_t(Nc, true), 
    pseq(Nc0,Nc), 
    drop(Xs,N,Nc0,Ys).

% P17 (*) Split a list into two parts; the length of the first part is given.
%    Do not use any predefined predicates.
%
%    Example:
%    ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
%    L1 = [a,b,c]
%    L2 = [d,e,f,g,h,i,k]
split(L,0,[],L).
split([X|Xs],N,[X|Ys],L2) :- 
    pseq(N0,N), 
    split(Xs,N0,Ys,L2).


% P18 (**) Extract a slice from a list.
%    Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
%
%    Example:
%    ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
%    X = [c,d,e,f,g]
slice([X|Xs],1,End,[X|Ys]) :-
    pseq(End0,End), 
    slice(Xs,End0,Ys).
slice([_|Xs],Start,End,Ys) :-
    pseq(Start0,Start), 
    pseq(End0,End), 
    slice(Xs,Start0,End0,Ys).

slice([X|_],1,[X]).
slice([X|Xs],End,[X|Ys]) :- 
    pseq(End0,End), 
    slice(Xs,End0,Ys).

% P19 (**) Rotate a list N places to the left.
%    Examples:
%    ?- rotate([a,b,c,d,e,f,g,h],3,X).
%    X = [d,e,f,g,h,a,b,c]
%
%    ?- rotate([a,b,c,d,e,f,g,h],-2,X).
%    X = [g,h,a,b,c,d,e,f]
%
%    Hint: Use the predefined predicates length/2 and append/3, as well as the result of problem P17.