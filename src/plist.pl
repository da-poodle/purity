:- ensure_loaded(purity).

:- multifile(pcompare/4).
:- multifile(ptype/2).

ptype([], plist).
ptype([_|_], plist).
 
pcompare(plist, L1, L2, C) :-
    plist_compare(L1, L2, C).
 
plist_compare([],S,C) :- plist_compare_0(S, C).
plist_compare([A|T1], S, C) :- plist_compare_1(S, [A|T1], C).

plist_compare_0([], =).
plist_compare_0([_|_], <).

plist_compare_1([], _, >).
plist_compare_1([A|T1], [B|T2], C) :-
        pcompare(B, A, TC), 
        plist_compare_cond(TC, T2, T1, C).

plist_compare_cond(<, _, _, <).
plist_compare_cond(>, _, _, >).
plist_compare_cond(=, T1, T2, C) :- 
       plist_compare(T1, T2, C).

% plength(List, Length).
plength([], zero).
plength([_|T], c(Z)) :- plength(T, Z).


% pnth0(Nth, Val, List).
pnth0(zero, V, [V|_]).
pnth0(c(Z), V, [_|T]) :-
    pnth0(Z, V, T).


% pnth1(Nth, Val, List).
pnth1(c(zero), V, [V|_]).
pnth1(c(c(Z)), V, [_|T]) :-
    pnth1(c(Z), V, T).

% pmemberchk(Element, List, Truth).
pmemberchk(Element, List, Truth) :-
   pmemberchk_(List, Element, Truth).

pmemberchk_t([], _, false).
pmemberchk_t([X|Xs], Element, Truth) :-
   pif( eq(X, Element), 
        unify(Truth, true), 
        pmemberchk_t(Xs, Element, Truth) 
    ).

% pmemberchk(Element, List).
pmemberchk(A,[B|T]) :-
	pcompare(A, B, C ),
	pmemberchk_(C,A,T).

pmemberchk_(=,_,_).
pmemberchk_(>,A,L) :-
	pmemberchk(A,L).
pmemberchk_(<,A,L) :-
	pmemberchk(A,L).



% non_member(Element, List).
non_member(A, L) :- non_member_(L, A).

non_member_([], _).
non_member_([A|T], B) :-
	pdif(A, B),
	non_member_(T, B).


% psublist(List, Before, Length, After, SubList).
psublist([A|T], zero, c(Len), End, [A|St]) :-
    psublist_(T, Len, End, St).

psublist([_|T], c(Start), Len, End, Sub) :-
    psublist(T, Start, Len, End, Sub).

psublist_(L, zero, End, []) :-
    plength(L, End).

psublist_([A|T], c(Len), End, [A|St]) :-
        psublist_(T, Len, End, St).

% remove_dups(List, NoDups).
remove_dups([],[]).
remove_dups([A|T],R) :-
	pmember(A,T),
	remove_dups(T,R).
remove_dups([A|T],[A|R]) :-
	non_member(A,T),
	remove_dups(T,R).

% list_join(ListOfLists, DelimList, ResultList).
list_join(Lol, Dl, Rl) :-
    list_join(Lol, [], Dl, Rl).

list_join([], [], _, []). % joining an empty list?
list_join([E|T], Prev, Dl, Rl) :-
    list_join2(T, E, Prev, Dl, Rl).

list_join2([], E, Prev, _, Rl) :-
    pappend(Prev, E, Rl).
list_join2([E2|T], E, Prev, Dl, Rl) :-
    pappend(Prev, E, PrevE),
    pappend(PrevE, Dl, Joined),
    list_join([E2|T], Joined, Dl, Rl).


% pfilter(Goal, Elements, Filtered).
%
% adapted from https://github.com/mthom/scryer-prolog/blob/master/src/lib/reif.pl
pfilter(Goal, Elements, Filtered) :-
   pfilter_(Elements, Goal, Filtered).

pfilter_([], _, []).
pfilter_([E|Es], Goal, Filtered) :-
   pif(pcall(Goal, E), unify(Filtered, [E|Fs]), unify(Filtered, Fs)),
   pfilter_(Es, Goal, Fs).

% ppartition(Goal, List, Included, Excluded).
ppartition(G,L,I,E) :-
    ppartition_(L,G,I,E).

ppartition_([],_,[],[]).
ppartition_([A|T],G,I,E) :-
    pcall(G,A,B),
    ppartition__(B,[A|T],G,I,E).

ppartition__(true, [A|T],G,[A|I],E) :- 
    ppartition_(T,G,I,E).
ppartition__(false,[A|T],G,I,[A|E]) :-
    ppartition_(T,G,I,E).


% pinclude(Goal, List, Included).
pinclude(G, L, I) :-
    ppartition(G, L, I, _).

% pexclude(Goal, List, Excluded).
pexclude(G, L, E) :-
    ppartition(G, L, _, E).

% psort(Domain, List, Sordered)
psort(L, S) :-
	psame_length(L, S),
    psort_(L, S).

psort_([], []).
psort_([A|T], S) :-
    psort_1(T, A, S).

psort_1([], A, [A]).
psort_1([A2|T], A, S) :-
	split([A,A2|T], L, R),
	psort_(L, SL),
	psort_(R, SR),
	pmerge(SL, SR, S).


% split(List, LeftPart, RightPart )
split([], [], []).
split([A|T], R, O) :-
    split_(T, A, R, O).

split_([], A, [A], []).
split_([A2|T], A, [A|LT], [A2|RT]) :-
    split(T, LT, RT).

% pmerge(LeftSide, RightSide, Merged, Domain)
pmerge( [], R, M ) :- pmerge_0( R, [], M ).
pmerge( [L|Lt], R, M ) :- pmerge_x( R, [L|Lt], M ).

pmerge_0( [], [], [] ).
pmerge_0( [R|Rt], [], [R|Rt] ).

pmerge_x( [], [L|Lt], [L|Lt] ).
pmerge_x( [R|Rt], [L|Lt], T ) :-
	pcompare( L, R, C ),
	pmerge_( C, [L|Lt], [R|Rt], T ).

pmerge_( =, [L|Lt], [R|Rt], [L,R|T] ) :-
	pmerge( Lt, Rt, T ).
pmerge_( <, [L|Lt], [R|Rt], [L|T] ) :-
	pmerge( Lt, [R|Rt], T ).
pmerge_( >, [L|Lt], [R|Rt], [R|T] ) :-
	pmerge( [L|Lt], Rt, T ).


% p_is_set/1
pset_empty(set([])).

% subset_t(SubSet, Set, Truth)
subset_t(set(Sub), set(Set), T) :-
    subset_(Sub, Set, T).

subset_([],_,true).
subset_([A|Sub],Set,T) :-
    subset__(Set, [A|Sub],T).

subset__([],_,false).
subset__([B|Set],[A|Sub],T) :-
    pcompare(A,B,C),
    subset_c(C,[A|Sub],[B|Set],T).

subset_c(=,[_|Sub],[_|Set],T) :- 
    subset_(Sub,Set,T).
subset_c(<,_,_,false).
subset_c(>,Sub,[_|Set],T) :- 
    subset_(Sub,Set,T).

% list_set(List, Set).
list_set(L, set(Set)) :-
    psort(L,B),
    remove_dups_sorted(B,Set).

remove_dups_sorted([], []).
remove_dups_sorted([A|T],R) :-
    remove_dups_sorted_(T,A,R).

remove_dups_sorted_([],A,[A]).
remove_dups_sorted_([B|T],A,R) :-
    pdif(A,B,C),
    remove_dups_sorted_(C,[A,B|T],R).

remove_dups_sorted_(true,[A,B|T],[A|R]) :-
    remove_dups_sorted([B|T],R).
remove_dups_sorted_(false,[A,A|T],R) :-
    remove_dups_sorted([A|T],R).


% pmember/2
pmember(A,[A|_]).
pmember(A,[_|T]) :-
    pmember(A,T).

% pappend/3
pappend([], A, A).
pappend([A|B], C, [A|D]) :-
    pappend(B, C, D).

% pappend/2
pappend(ListOfLists, List) :-
    is_list(ListOfLists),    
    pappend_(ListOfLists, List).

pappend_([], []).
pappend_([L|Ls], As) :-
    pappend(L, Ws, As),
    pappend_(Ls, Ws).

% pselect/3
pselect(A, [A|B], B).
pselect(B, [A|C], [A|D]) :-
    pselect(B, C, D).

% pselect/4
pselect(A,[A|C], B, [B|C]).
pselect(C, [A|B], D, [A|E]) :-
    pselect(C, B, D, E).

% preverse/2
% solution taken from https://courses.cs.washington.edu/courses/cse341/10wi/clpr/difference_lists.clpr
preverse(Xs,Rs) :- reverse_dl(Xs,Rs-[]).

reverse_dl([],T-T).
reverse_dl([X|Xs],Rs-T) :- reverse_dl(Xs,Rs-[X|T]).

% ppermutation/2
ppermutation([],[]).
ppermutation(A,[E|R]) :-
	pselect(E,A,B),
	ppermutation(B,R).

% plast/2
plast([A],A).
plast([_|T],A) :-
    plast(T,A).


% pprefix/2
pprefix([], _).
pprefix([A|B], [A|C]) :-
    pprefix(B, C).

% psame_length
psame_length([],[]).
psame_length([_|A],[_|B]) :-
    psame_length(A,B).