:- use_module('../prolog/domain_pchar.pl').
:- use_module('../prolog/domain_seq.pl').
:- use_module('../prolog/purity.pl').
:- use_module('../prolog/plist.pl').
:- use_module('../prolog/pseq.pl').

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
my_flatten([], []).
my_flatten([A|T], Result) :-
    flatten_by_type(A, L),
    my_flatten(T, Next),
    append(L, Next, Result).

flatten_by_type(A, R) :- plist(A), my_flatten(A, R).
flatten_by_type(A, [A]) :- domain(pchar, A, _).

% P08 (**) Eliminate consecutive duplicates of list elements.
%    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
%
%    Example:
%    ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%    X = [a,b,c,a,d,e]
compress(List, Compressed) :-
    maplist(domain(_), List, Mapped),
    remove_dups2(Mapped, Result),
    maplist(domain(_), Compressed, Result).

remove_dups2([], []).
remove_dups2([A|T], [Dedupped|Result]) :-
    remove_seq([A|T], Dedupped, Remaining),
    remove_dups2(Remaining, Result).

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
pack(List, Compressed) :-
    maplist(domain(pchar), List, Mapped),
    pack_(Mapped, Result),
    maplist(domain(plist(pchar)), Compressed, Result).

pack_([], []).
pack_([A|T], [Dedupped|Result]) :-
    pack_similar([A|T], Dedupped, Remaining),
    pack_(Remaining, Result).

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
    maplist(domain(pchar), List, Mapped),
    pack_(Mapped, Packed),
    maplist(encoded_counted, Packed, Encoded).

encoded_counted(Packed, [N, D]) :-
    encoded_counted(Packed, A, N),
    domain(pchar, D, A).

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
    maplist(domain(pchar), List, Mapped),
    pack_(Mapped, Packed),
    maplist(encoded_counted_mod, Packed, Encoded).

encoded_counted_mod(Packed, R) :-
    encoded_counted_mod(Packed, A, N),
    domain(pchar, D, A),
    pif(eq(N, 1), R = D, R = [N, D]).

encoded_counted_mod([A], A, 1).
encoded_counted_mod([A|T], A, N) :-
    pseq(N0, N),
    encoded_counted_mod(T, A, N0).