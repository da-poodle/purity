% List Library
:- module(plist, [
    plist/1,
    plength/2,
    pnth0/3,
    pnth1/3,
    pmemberchk/3,    
    psublist/5,
    ppartition/4,
    pinclude/3,
    pexclude/3,
    list_in_domain/2,
    non_member/3,
    remove_dups/2,
    list_join/3,
    psort/3
]).

:- use_module(purity).

:- multifile purity:pcompare/4.

purity:pcompare(plist(D), A, B, C) :- plist_compare(A, B, C, D). 

/*
% member/2
member(A,[A|_]).
member(A,[_|T]) :-
    member(A,T).

% append/3
append([], A, A).
append([A|B], C, [A|D]) :-
    append(B, C, D).

% select/3
select(A, [A|B], B).
select(B, [A|C], [A|D]) :-
    select(B, C, D).

% select/4
select(A,[A|C], B, [B|C]).
select(C, [A|B], D, [A|E]) :-
    select(C, B, D, E).

% reverse/2
reverse(A,B) :-
    reverse(A,[],B).

reverse([], A, A).
reverse([B|A], C, R) :-
    reverse(A, [B|C], R).

% permutation/2
permutation([],[]).
permutation(A,[E|R]) :-
	select(E,A,B),
	permutation(B,R).

% last/2
last([A],A).
last([_|T],A) :-
    last(T,A).


% prefix/2
prefix([], _).
prefix([A|B], [A|C]) :-
    prefix(B, C).

% same_length
psame_length([],[]).
psame_length([_|A],[_|B]) :-
    psame_length(A,B).
*/


% plist(L).
%
% holds if L is a valid list
%
plist([]).
plist([_|_]).


% plist_compare(List1, List2, Comparator, Domain).
%
% compare two lists of type 'Domain' 
% Comparator contains either =, < or >.
% comparisons are in relation to the first list
% eg: if List1 = 3 and List2 = 4 then List1 < List2
%
% D = domain
% C = the comparator operator
% A = list1 element to compare
% B = list2 element to compare
% T1 = tail of list1
% T2 = tail of list2
% S = list2
% TC = temporary compartor
%
plist_compare([],S,C, _) :- plist_compare_0(S, C).
plist_compare([A|T1], S, C, D) :- plist_compare_1(S, [A|T1], C, D).

plist_compare_0([], =).
plist_compare_0([_|_], <).

plist_compare_1([], _, >, _).
plist_compare_1([A|T1], [B|T2], C, D) :-
        pcompare(D, B, A, TC), 
        plist_compare_cond(TC, T2, T1, C, D).

plist_compare_cond(<, _, _, <, _).
plist_compare_cond(>, _, _, >, _).
plist_compare_cond(=, T1, T2, C, D) :- 
        plist_compare(T1, T2, C, D).


% plength(List, Length).
%
% Length is the number of elements in List using the punary domain.
%
% T = the tail of List
% Z = recursive part of Length
%
plength([], zero).
plength([_|T], c(Z)) :- plength(T, Z).


% pnth0(Nth, Val, List).
%
% Val is the Nth element in List starting at zero.
% Nth is a punary number.
%
% V = Val
% T = the tail of List
% Z = the recursive part of Nth.
%
pnth0(zero, V, [V|_]).
pnth0(c(Z), V, [_|T]) :-
    pnth0(Z, V, T).


% pnth1(Nth, Val, List).
%
% Val is the Nth element in List starting at c(zero).
% Nth is a punary number.
%
% V = Val
% T = the tail of List
% Z = the recursive part of Nth.
%
pnth1(c(zero), V, [V|_]).
pnth1(c(c(Z)), V, [_|T]) :-
    pnth1(c(Z), V, T).


% list_in-domain(Domain, List).
%
% Holds if all elements of List are in Domain.
%
% D = Domain
% L = List
% A = an element of List
% T = the tail of List
%
list_in_domain_( [], _ ).
list_in_domain_( [A|T], D ) :- 
	pcompare( D, A, A, = ),
	list_in_domain_( T, D ).

list_in_domain( D, L ) :- 
    list_in_domain( L, D ).


% pmemberchk(Domain, Element, List).
%
% check if Element exists in List once only.
%
% D = Domain
% A = Element 
% L = List
% B = an element of List that doesn't match Element
% T = the tail of List
% C = the comparison operator
%
pmemberchk_(=,_,_,_).
pmemberchk_(>,A,L,D) :-
	pmemberchk(D,A,L).
pmemberchk_(<,A,L,D) :-
	pmemberchk(D,A,L).

pmemberchk(D, A,[B|T]) :-
	pcompare(D, A, B, C ),
	pmemberchk_(C,A,T,D).


% non_member(Domain, Element, List).
%
% Holds if Element is not an element in List.
% the Element and List must be a member of Domain.
%
% D = Domain
% L = List
% A = Element
% B = an element of List that is different to Element
% T = the tail of List
%
non_member(D, A, L) :- non_member_(L, A, D).

non_member_([], _, _).
non_member_([A|T], B, D) :-
	pdif(D, A, B),
	non_member_(T, B, D).


% psublist(List, Before, Length, After, SubList).
%
% SubList is contained within List in the same sequence.
% Before is the number of elements before SubList
% Length is the number of elements in SubList
% After is the number of elements after SubList
%
% A = an element of List 
% T = the tail of List
% St = the tail of SubList
% Len = Length
% End = After
% Start = Before
% Sub = SubList
%
% match! the start is a zero from now on    
psublist([A|T], zero, c(Len), End, [A|St]) :-
    psublist_(T, Len, End, St).

% no match yet, nothing to see here, increment the start
psublist([_|T], c(Start), Len, End, Sub) :-
    psublist(T, Start, Len, End, Sub).

% single character substring, ok, one length?
psublist_(L, zero, End, []) :-
    plength(L, End).

% in the match, counting...
psublist_([A|T], c(Len), End, [A|St]) :-
        psublist_(T, Len, End, St).


% remove_dups(List, NoDups).
%
% NoDups is List without any repeating items.
%
% A = an element of List or NoDups
% T = the tail of List
% R = the tail of NoDups
%
remove_dups([],[]).
remove_dups([A|T],R) :-
	member(A,T),
	remove_dups(T,R).
remove_dups([A|T],[A|R]) :-
	non_member(_,A,T),
	remove_dups(T,R).


% list_join(ListOfLists, DelimList, ResultList).
%
% ResultList is the ListOfLists flattened with DelimList separating each list
% eg: ListOfLists = [[a],[b],[c]], DelimList = [','], ResultList = [a,',',b,',',c]
%
% Lol = ListOfLists
% Dl = DelimList
% Rl = ResultList
% E = The first element in ListOfLists
% E2 = The second element in ListOfLists
% Prev = the current accumulated ResultList
% PrevE = E appened to Prev
% Joined = Dl appended to PrevE 
%
list_join(Lol, Dl, Rl) :-
    list_join(Lol, [], Dl, Rl).

list_join([], [], _, []). % joining an empty list?
list_join([E|T], Prev, Dl, Rl) :-
    list_join2(T, E, Prev, Dl, Rl).

list_join2([], E, Prev, _, Rl) :-
    append(Prev, E, Rl).
list_join2([E2|T], E, Prev, Dl, Rl) :-
    append(Prev, E, PrevE),
    append(PrevE, Dl, Joined),
    list_join([E2|T], Joined, Dl, Rl).


% ppartition(Goal, List, Included, Excluded).
%
% Using Goal as the decider, the elements of list are either in Included or Excluded.
% Included and Excluded are also lists. 
%
% G = Goal - a pb_call formatted goal
% L = List
% I = Included
% E = Excluded
% A = the first element of List
% B = the binary result of Goal
% T = the tail of List
% 
ppartition(G,L,I,E) :-
    ppartition_(L,G,I,E).

ppartition_([],_,[],[]).
ppartition_([A|T],G,I,E) :-
    call(G,A,B),
    ppartition__(B,[A|T],G,I,E).

ppartition__(true, [A|T],G,[A|I],E) :- 
    ppartition_(T,G,I,E).
ppartition__(false,[A|T],G,I,[A|E]) :-
    ppartition_(T,G,I,E).


% pinclude(Goal, List, Included).
%
% calls partition/4 and keeps the Included part.
%
pinclude(G, L, I) :-
    ppartition(G, L, I, _).


% pexclude(Goal, List, Excluded).
%
% calls partition/4 and keeps the Excluded part.
%
pexclude(G, L, E) :-
    ppartition(G, L, _, E).


% psort(Domain, List, Sordered)
%
% Sorted is an ordered version of List.
% all elements in  List and Sorted must be in Domain. 
%
% D = Domain
% L = List
% S = Sorted
% A = the first element of List
% A2 = the second element of list
% T = the tail of List
%
% L = the left split part of List
% R = the right split part of List
% SL = the sorted version of L
% SR = the sorted version of R
%
psort(D, L, S) :-
	same_length(L, S),
	psort_(L, S, D).

psort_([], [], _).
psort_([A|T], S, D) :-
    psort_1(T, A, S, D).

psort_1([], A, [A], _).
psort_1([A2|T], A, S, D) :-
	split([A,A2|T], L, R),
	psort_(L, SL, D),
	psort_(R, SR, D),
	pmerge(SL, SR, S, D).


% split(List, LeftPart, RightPart )
%
% Alternate elements of LIST in L and R
%
% A = the first or an element of List
% A2 = the second element of List
% T = the tail of List
% LT = the tail of LeftPart 
% RT = the tail of RightPart
% 
split([], [], []).
split([A|T], R, O) :-
    split_(T, A, R, O).

split_([], A, [A], []).
split_([A2|T], A, [A|LT], [A2|RT]) :-
    split(T, LT, RT).


% merge(LeftSide, RightSide, Merged, Domain)
% 
% assuming LeftSide and RightSide are sorted, Merged is the sorted merge of the two
% all elements in LeftSide, RightSide and Merged must be in Domain.
%
% L = an element of LeftSide
% Lt = the tail of LeftSide
% R = an element of RigtSide
% Rt = the tail of RightSide
% T = the tail of Merged
% D = Domain
% C = a comparison operator
%
pmerge( [], R, M, _ ) :- pmerge_0( R, [], M ).
pmerge( [L|Lt], R, M, D ) :- pmerge_x( R, [L|Lt], M, D ).

pmerge_0( [], [], [] ).
pmerge_0( [R|Rt], [], [R|Rt] ).

pmerge_x( [], [L|Lt], [L|Lt], _ ).
pmerge_x( [R|Rt], [L|Lt], T, D ) :-
	pcompare( D, L, R, C ),
	pmerge_( C, [L|Lt], [R|Rt], T, D ).

pmerge_( =, [L|Lt], [R|Rt], [L,R|T], D ) :-
	pmerge( Lt, Rt, T, D ).
pmerge_( <, [L|Lt], [R|Rt], [L|T], D ) :-
	pmerge( Lt, [R|Rt], T, D ).
pmerge_( >, [L|Lt], [R|Rt], [R|T], D ) :-
	pmerge( [L|Lt], Rt, T, D).

