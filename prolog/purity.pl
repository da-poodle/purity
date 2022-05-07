:- module(purity, [
    
    % comparison
    pcompare/3,
    ptype/2,

    pdif/2,
    pdif_t/3,
    
    pif/3, 
    (',')/3, 
    (;)/3,
    
    (=)/3,
    eq/2,
    eq/3,
    
    (<)/3,
    lt/2,
    lt/3,
    
    (<=)/3,
    lte/2,
    lte/3,

    (>)/3,
    gt/2,
    gt/3,

    (>=)/3,
    gte/2,
    gte/3,

    % list
    plength/2,
    pnth0/3,
    pnth1/3,
    pmember/3,
    pmemberchk/2,    
    psublist/5,
    pfilter/3,
    ppartition/4,
    pinclude/3,
    pexclude/3,
    non_member/2,
    remove_dups/2,
    list_join/3,
    psort/2,

    % set
    pset_empty/1,
    list_set/2,
    set_list/2,
    subset_t/3,
    punion/3,
    pdifference/3,
    pintersection/3,

    % unary
    unary/1,
    unary_compare/3,
    pow/3,
    string_unary/2,
    unary_string/2,
    add/3,
    mul/3,
    div/4,

    % hex
    hex_compare/3,
    hex2_compare/3,

    % pchar
    pchar_compare/3,
    pchar_upper/2,
    pchar_lower/2,
    pchar_type/2,
    pchar_code/2,
    pchar_hex2/2,

    % string
    pstr_upper/2,
    pstr_lower/2,
    pstr_split/3,
    pstr_join/3,
    pstr_contains/2,
    pstr_contains/3,
    pstr_prefix/2,
    pstr_prefix/3,
    pstr_trim/2,
    pstr_replace/4
]).

% pcompare(TermA, TermB, Comparator) - 
%
% Comparator is one of <, > or =
% Comparator is the comparison between TermA and TermB
% both TermA and TermB must be in Domain
%
:- multifile(pcompare/3).
:- multifile(ptype/2).

:- meta_predicate(pif(1, 0, 0)).

/*
    TYPES
*/

ptype(A, domain) :- pcompare(A, A, =).
ptype(hex(_), hex).
ptype(hex2(_), hex2).
ptype([], list).
ptype([_|_], list).
ptype(set([]), set).
ptype(set([_,_]), set).
ptype(zero, unary).
ptype(c(_), unary).


% if(Goal, TrueGoal, FalseGoal).
%
% Goal is a callable with the last argument as a boolean (true or false).
% if Goal results in true then TrueGoal is called.
% if Goal results in false then FalseGoal is called.
%
pif(Goal, TrueGoal, FalseGoal) :-
    call(Goal, R),
    pif_(R, TrueGoal, FalseGoal).

pif_(true, Goal, _) :- call(Goal).
pif_(false, _, Goal) :- call(Goal).

','(A, B, R) :-
    call(A, T),
    conj_(T, B, R).

conj_(true, B, R) :-
    call(B, R).
conj_(false, _, false).

';'(A, B, T) :-
    call(A, T)
    ; 
    call(B, T).


pdif_t(A,B,R) :-
    pcompare(A,B,C),
    pdif_(C,R).

pdif_(=,false).
pdif_(<,true).
pdif_(>,true).

pdif(A,B) :- pdif_t(A, B, true).

% eq/2, eq/3, (=)/3
eq(A, B, T) :- compare_with_states(eq_, A, B, T).
eq(A, B) :-  eq(A, B, true).
=(A, B, T) :- eq(A, B, T).

eq_(=, true).
eq_(<, false).
eq_(>, false).

% lt/2, lt/3, (<)/3
lt(A, B, T) :- compare_with_states(lt_, A, B, T).
lt(A, B) :- lt(A, B, true).
<(A, B, T) :- lt(A, B, T).

lt_(=, false).
lt_(<, true).
lt_(>, false).

% lte/2, lte/3, (<=)/3
lte(A, B, T) :- compare_with_states(lte_, A, B, T).
lte(A, B) :- lte(A, B, true).
<=(A, B, T) :- lte(A, B, T).

lte_(=, true).
lte_(<, true).
lte_(>, false).

% gt/2, gt/3, (>)/3
gt(A, B, T) :- compare_with_states(gt_, A, B, T).
gt(A, B) :- gt(A, B, true).    
>(A, B, T) :- gt(A, B, T).

gt_(=, false).
gt_(<, false).
gt_(>, true).

% gte/2, gte/3, (>=)/3
gte(A, B, T) :- compare_with_states(gte_, A, B, T).
gte(A, B) :- gte(A, B, true).
>=(A, B, T) :- gte(A, B, T).

gte_(=, true).
gte_(<, false).
gte_(>, true).

compare_with_states(StateGoal, A, B, Truth) :-  
    pcompare(A, B, C),
    call(StateGoal, C, Truth).


/*
    LISTS
*/
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
% solution taken from https://courses.cs.washington.edu/courses/cse341/10wi/clpr/difference_lists.clpr
preverse(Xs,Rs) :- reverse_dl(Xs,Rs-[]).

reverse_dl([],T-T).
reverse_dl([X|Xs],Rs-T) :- reverse_dl(Xs,Rs-[X|T]).

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

% plist_compare(List1, List2, Comparator).
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

% pmember(Element, List, Truth).
pmember(Element, List, Truth) :-
   pmember_(List, Element, Truth).

pmember_([], _, false).
pmember_([X|Xs], Element, Truth) :-
   pif( eq(X, Element), 
        Truth = true, 
        pmember_(Xs, Element, Truth) 
    ).

% pmemberchk(Element, List).
pmemberchk_(=,_,_).
pmemberchk_(>,A,L) :-
	pmemberchk(A,L).
pmemberchk_(<,A,L) :-
	pmemberchk(A,L).

pmemberchk(A,[B|T]) :-
	pcompare(A, B, C ),
	pmemberchk_(C,A,T).


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
	member(A,T),
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
    append(Prev, E, Rl).
list_join2([E2|T], E, Prev, Dl, Rl) :-
    append(Prev, E, PrevE),
    append(PrevE, Dl, Joined),
    list_join([E2|T], Joined, Dl, Rl).


% pfilter(Goal, Elements, Filtered).
%
% adapted from https://github.com/mthom/scryer-prolog/blob/master/src/lib/reif.pl
pfilter(Goal, Elements, Filtered) :-
   pfilter_(Elements, Goal, Filtered).

pfilter_([], _, []).
pfilter_([E|Es], Goal, Filtered) :-
   pif(call(Goal, E), Filtered = [E|Fs], Filtered = Fs),
   pfilter_(Es, Goal, Fs).

% ppartition(Goal, List, Included, Excluded).
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
pinclude(G, L, I) :-
    ppartition(G, L, I, _).

% pexclude(Goal, List, Excluded).
pexclude(G, L, E) :-
    ppartition(G, L, _, E).

% psort(Domain, List, Sordered)
psort(L, S) :-
	same_length(L, S),
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
    pdif_t(A,B,C),
    remove_dups_sorted_(C,[A,B|T],R).

remove_dups_sorted_(true,[A,B|T],[A|R]) :-
    remove_dups_sorted([B|T],R).
remove_dups_sorted_(false,[A,A|T],R) :-
    remove_dups_sorted([A|T],R).

% set_list(Set, List).
set_list(set(List), List).


% punion(L1,L2,Union).
punion(set(S1),set(S2),set(S3)) :-
    punion_(S1,S2,S3).
 
punion_([],B,B).
punion_([A|At],B,R) :-
    punion_1(B,[A|At],R).

punion_1([],A,A).
punion_1([B|Bt],[A|At],R) :-
    pcompare(A,B,C),
    punion_c(C,[A|At],[B|Bt],R).

punion_c(=,[A|At],[A|Bt],[A|R]) :- 
    punion_(At,Bt,R).
punion_c(<,[A|At],[B|Bt],[A|R]) :- 
    punion_(At,[B|Bt],R).
punion_c(>,[A|At],[B|Bt],[B|R]) :- 
    punion_([A|At],Bt,R).


% difference(Set1, Set2, Difference).
pdifference(set(S1),set(S2),set(S3)) :-
    pdifference_(S1,S2,S3).
 
pdifference_([],_,[]).
pdifference_([A|At],B,R) :-
    pdifference_1(B,[A|At],R).

pdifference_1([],A,A).
pdifference_1([B|Bt],[A|At],R) :-
    pcompare(A,B,C),
    pdifference_c(C,[A|At],[B|Bt],R).

pdifference_c(=,[A|At],[A|Bt],R) :- 
    pdifference_(At,Bt,R).
pdifference_c(<,[A|At],[B|Bt],[A|R]) :- 
    pdifference_(At,[B|Bt],R).
pdifference_c(>,[A|At],[_|Bt],[A|R]) :- 
    pdifference_([A|At],Bt,R).


% pintersection(Set1,Set2,Intersection).
pintersection(set(S1),set(S2),set(S3)) :-
    pintersection_(S1,S2,S3).
 
pintersection_([],_,[]).
pintersection_([A|At],B,R) :-
    pintersection_1(B,[A|At],R).

pintersection_1([],_,[]).
pintersection_1([B|Bt],[A|At],R) :-
    pcompare(A,B,C),
    pintersection_c(C,[A|At],[B|Bt],R).

pintersection_c(=,[A|At],[A|Bt],[A|R]) :- 
    pintersection_(At,Bt,R).
pintersection_c(<,[_|At],[B|Bt],R) :- 
    pintersection_(At,[B|Bt],R).
pintersection_c(>,[A|At],[_|Bt],R) :- 
    pintersection_([A|At],Bt,R).

/*
    PUNARY
*/

% unary_compare(Unary1, Unary2, Comparator).
unary_compare(zero, U2, C) :- unary_compare_z(U2, C).
unary_compare(c(Z), U2, C) :- unary_compare_c(U2, c(Z), C).

unary_compare_z(zero, =).
unary_compare_z(c(_), <).

unary_compare_c(zero, _, >).
unary_compare_c(c(Z2), c(Z1), C) :- 
	unary_compare(Z1, Z2, C).

% unary(Unary).
unary(zero).
unary(c(Z)) :- unary(Z).

% add(UnaryA, UnaryB, Sum).
add(zero, Z, Z).
add(c(X), Y, c(Z)) :- 
	add(X, Y, Z).

% mul(UnaryA, UnaryB, Product)
mul(zero, _, zero).
mul(c(X), Y, R) :- 
	add(Y, S, R), 
	mul(X, Y, S).

% div(Divide, By, Quotient, Remainder)
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
	pchar_compare(A, N, C),
	d_to_u_(C, N, T, U).

d_to_u_(=, _, _, zero).
d_to_u_(>, N, T, c(Z)) :-
	d_to_u_(T, N, Z).
d_to_u_(<, N, T, c(Z)) :-
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
	
% string_unary(String, Unary).
string_unary(D, C) :- 
	reverse(D, DR),
	u_dec_unary_(DR, c(zero), zero, C).

u_dec_unary_([], _, Sum, Sum).
u_dec_unary_([D|T], Mult, Sum, R) :-	
	u_to_d(X, D),
	mul(Mult, X, Num),
	add(Sum, Num, Sum1),
	
	unary_10(M),	
	mul(Mult, M, NewMult),
	
	u_dec_unary_(T, NewMult, Sum1, R).

	
% unary_string(Unary, String).
unary_string(U, S) :- 
	u_unary_dec(U, c(zero), R),
	reverse(R, S).

u_unary_dec(zero, _, []).
u_unary_dec(c(Z), Mul, [V|T]) :-
	unary_10(U_10),
	mul(Mul, U_10, NewMul),
	div(c(Z), NewMul, LeftOver, R),
	u_to_d(R, V),	
	u_unary_dec(LeftOver, NewMul, T).


/*
    PSTRING
*/

% pstr_upper(String, Upper).
pstr_upper([], []).
pstr_upper([C|T], [U|R]) :-
    ch_map(C, _, U),
    pstr_upper(T, R).

% pstr_lower(String, Lower).
pstr_lower([], []).
pstr_lower([C|T], [L|R]) :-
    ch_map(C, L, _),
    pstr_lower(T, R).

% pstr_replace(String, Find, Replace, Replaced).
pstr_replace([], _, _, []).
pstr_replace([A|T], F, R, Rp) :-
    pstr_prefix(F, [A|T], IsPrefix),
    pstr_replace_(IsPrefix, [A|T], F, R, Rp).

pstr_replace_(true, Orig, F, R, Rp) :-
    append(F, Remaining, Orig),
    append(R, Remaining, Rp).
pstr_replace_(false, [A|T], F, R, [A|Rpt]) :-
    pstr_replace(T, F, R, Rpt).

% pstr_trim(UnTrimmed, Trimmed).
pstr_trim([A|T], Out) :-
    pchar_type(A, Type),
    pstr_trim_start(Type, A, T, Out).

pstr_trim_start(whitespace, _, T, Out) :-  pstr_trim(T, Out).
pstr_trim_start(alpha, A, T, [A|Out]) :- pstr_trim_end(T, Out).
pstr_trim_start(digit, A, T, [A|Out]) :- pstr_trim_end(T, Out).
pstr_trim_start(symbol, A, T, [A|Out]) :- pstr_trim_end(T, Out).

pstr_trim_end([], []).
pstr_trim_end([A|T], Trimmed) :-
    pstr_all_whitespace([A|T], R),
    pstr_trim_end_(R, A, T, Trimmed).

pstr_trim_end_(true, _, _, []).
pstr_trim_end_(false, A, T, [A|End]) :- pstr_trim_end(T, End).

pstr_all_whitespace([], true).
pstr_all_whitespace([A|T], R) :-
    pchar_type(A, Type),
    pstr_all_whitespace_(Type, T, R).

pstr_all_whitespace_(whitespace, T, R) :- pstr_all_whitespace(T, R).
pstr_all_whitespace_(alpha, _, false).
pstr_all_whitespace_(digit, _, false).
pstr_all_whitespace_(symbol, _, false).

% pstr_split(StrToSplit, DelimiterChar, Split).
pstr_split(A, D, B) :-
    ptype(D, char),
    pstr_split_(A, D, B).

pstr_split_([], _, []).
pstr_split_([A|T], D, Split) :-
    pdif_char(A,D,B),
    pstr_split(B,A,T,D,Split).

pstr_split(false, D, T, D, Split) :-
    pstr_split_(T,D,Split).
pstr_split(true, A, T, D, [S|Rest]) :-
    find_split([A|T], D, Rem, S),
    pstr_split_(Rem, D, Rest). 

find_split([], _, [], []).
find_split([A|T], D, Rem, Split) :-
    pdif_char(A,D,B),
    find_split(B, A, T, D, Rem, Split).

find_split(false, D, T, D, T, []).
find_split(true, A, T, D, Rem, [A|Rest]) :-
    find_split(T, D, Rem, Rest).

% pstr_join(ListOfStrings, Delimiter, Joined).
pstr_join(A, D, B) :-
    ptype(D, char),
    pstr_join_(A, D, B).

pstr_join_([], _, []).
pstr_join_([A|T], D, S) :-
    pstr_join(A, T, D, S).

pstr_join([], T, D, R) :-
    pstr_join2(T, D, R).
pstr_join([A|At], T, D, [A|R]) :-
    pstr_join(At, T, D, R).

pstr_join2([], _, []).
pstr_join2([N|T], D, [D|R]) :-
    pstr_join_([N|T], D, R).

% pstr_contains(String, SubString).
pstr_contains(String, SubString) :-
    pstr_contains(String, SubString, true).

% pstr_contains(String, SubString, Contains).
pstr_contains([], _, false). 
pstr_contains([A|T], B, C) :-
    pstr_prefix(B, [A|T], Prefix),
    pstr_contains_(Prefix, [A|T], B, C).

pstr_contains_(true, _, _, true).
pstr_contains_(false, [_|At], B, C) :-
    pstr_contains(At, B, C).

% pstr_prefix(Prefix, String).
pstr_prefix(Prefix, String) :-
    pstr_prefix(Prefix, String, true).

% pstr_prefix(Prefix, String, IsPrefix).
pstr_prefix([], _, true).
pstr_prefix([A|At], [B|Bt], IsPrefix) :-
    pchar_compare(A, B, C),
    pstr_prefix(C, At, Bt, IsPrefix).

pstr_prefix(=, At, Bt, B) :- 
    pstr_prefix(At, Bt, B).
pstr_prefix(<, _, _, false).
pstr_prefix(>, _, _, false).


/*
    PCHAR
*/
 pchar_compare(A, B, C) :-
    pchar_hex2(A, Ah),
    pchar_hex2(B, Bh),
    hex2_compare(Ah,Bh,C).
 
pdif_char(A,B,T) :-
    pchar_compare(A,B,C),
    pdif_char_(C,T).

pdif_char_(=,false).
pdif_char_(<,true).
pdif_char_(>,true).

 % pchar_upper(Char, UpperChar).
 pchar_upper(C, U) :- ch_map(C, _, U).
 
 % pchar_lower(Char, LowerChar).
 pchar_lower(C, L) :- ch_map(C, L, _).
 
 % ch_map(Char, Lower, Upper).
 ch_map( a, a, 'A').
 ch_map( b, b, 'B').
 ch_map( c, c, 'C').
 ch_map( d, d, 'D').
 ch_map( e, e, 'E').
 ch_map( f, f, 'F').
 ch_map( g, g, 'G').
 ch_map( h, h, 'H').
 ch_map( i, i, 'I').
 ch_map( j, j, 'J').
 ch_map( k, k, 'K').
 ch_map( l, l, 'L').
 ch_map( m, m, 'M').
 ch_map( n, n, 'N').
 ch_map( o, o, 'O').
 ch_map( p, p, 'P').
 ch_map( q, q, 'Q').
 ch_map( r, r, 'R').
 ch_map( s, s, 'S').
 ch_map( t, t, 'T').
 ch_map( u, u, 'U').
 ch_map( v, v, 'V').
 ch_map( w, w, 'W').
 ch_map( x, x, 'X').
 ch_map( y, y, 'Y').
 ch_map( z, z, 'Z').
 ch_map( 'A', a, 'A').
 ch_map( 'B', b, 'B').
 ch_map( 'C', c, 'C').
 ch_map( 'D', d, 'D').
 ch_map( 'E', e, 'E').
 ch_map( 'F', f, 'F').
 ch_map( 'G', g, 'G').
 ch_map( 'H', h, 'H').
 ch_map( 'I', i, 'I').
 ch_map( 'J', j, 'J').
 ch_map( 'K', k, 'K').
 ch_map( 'L', l, 'L').
 ch_map( 'M', m, 'M').
 ch_map( 'N', n, 'N').
 ch_map( 'O', o, 'O').
 ch_map( 'P', p, 'P').
 ch_map( 'Q', q, 'Q').
 ch_map( 'R', r, 'R').
 ch_map( 'S', s, 'S').
 ch_map( 'T', t, 'T').
 ch_map( 'U', u, 'U').
 ch_map( 'V', v, 'V').
 ch_map( 'W', w, 'W').
 ch_map( 'X', x, 'X').
 ch_map( 'Y', y, 'Y').
 ch_map( 'Z', z, 'Z').
 ch_map( ';', ';', ';').
 ch_map( ':', ':', ':').
 ch_map( '"', '"', '"').
 ch_map( '\'','\'','\'').
 ch_map( '\'','\'','\'').
 ch_map( '/','/','/').
 ch_map( '\\','\\','\\').
 ch_map( '<','<','<').
 ch_map( '>','>','>').
 ch_map( ',',',',',').
 ch_map( '.','.','.').
 ch_map( '!','!','!').
 ch_map( '@','@','@').
 ch_map( '#','#','#').
 ch_map( '$','$','$').
 ch_map( '%','%','%').
 ch_map( '^','^','^').
 ch_map( '&','&','&').
 ch_map( '*','*','*').
 ch_map( '(','(','(').
 ch_map( ')',')',')').
 ch_map( '[','[','[').
 ch_map( ']',']',']').
 ch_map( '{','{','{').
 ch_map( '}','}','}').
 ch_map( '|','|','|').
 ch_map( '-','-','-').
 ch_map( '_','_','_').
 ch_map( '+','+','+').
 ch_map( '=','=','=').
 ch_map( '0','0','0').
 ch_map( '1','1','1').
 ch_map( '2','2','2').
 ch_map( '3','3','3').
 ch_map( '4','4','4').
 ch_map( '5','5','5').
 ch_map( '6','6','6').
 ch_map( '7','7','7').
 ch_map( '8','8','8').
 ch_map( '9','9','9').
 ch_map( ' ',' ',' ').
 ch_map( '\n','\n','\n').
 ch_map( '\r','\r','\r').
 ch_map( '\t','\t','\t').
 
 % pchar_type(Char, Type).
 %
 % Type is one of alpha, digit, symbol, or whitespace
 % 
 pchar_type( a, alpha).
 pchar_type( b, alpha).
 pchar_type( c, alpha).
 pchar_type( d, alpha).
 pchar_type( e, alpha).
 pchar_type( f, alpha).
 pchar_type( g, alpha).
 pchar_type( h, alpha).
 pchar_type( i, alpha).
 pchar_type( j, alpha).
 pchar_type( k, alpha).
 pchar_type( l, alpha).
 pchar_type( m, alpha).
 pchar_type( n, alpha).
 pchar_type( o, alpha).
 pchar_type( p, alpha).
 pchar_type( q, alpha).
 pchar_type( r, alpha).
 pchar_type( s, alpha).
 pchar_type( t, alpha).
 pchar_type( u, alpha).
 pchar_type( v, alpha).
 pchar_type( w, alpha).
 pchar_type( x, alpha).
 pchar_type( y, alpha).
 pchar_type( z, alpha).
 pchar_type( 'A', alpha).
 pchar_type( 'B', alpha).
 pchar_type( 'C', alpha).
 pchar_type( 'D', alpha).
 pchar_type( 'E', alpha).
 pchar_type( 'F', alpha).
 pchar_type( 'G', alpha).
 pchar_type( 'H', alpha).
 pchar_type( 'I', alpha).
 pchar_type( 'J', alpha).
 pchar_type( 'K', alpha).
 pchar_type( 'L', alpha).
 pchar_type( 'M', alpha).
 pchar_type( 'N', alpha).
 pchar_type( 'O', alpha).
 pchar_type( 'P', alpha).
 pchar_type( 'Q', alpha).
 pchar_type( 'R', alpha).
 pchar_type( 'S', alpha).
 pchar_type( 'T', alpha).
 pchar_type( 'U', alpha).
 pchar_type( 'V', alpha).
 pchar_type( 'W', alpha).
 pchar_type( 'X', alpha).
 pchar_type( 'Y', alpha).
 pchar_type( 'Z', alpha).
 pchar_type( ';', symbol).
 pchar_type( ':', symbol).
 pchar_type( '"', symbol).
 pchar_type( '\'', symbol).
 pchar_type( '\'', symbol).
 pchar_type( '/', symbol).
 pchar_type( '\\', symbol).
 pchar_type( '<', symbol).
 pchar_type( '>', symbol).
 pchar_type( ',', symbol).
 pchar_type( '.', symbol).
 pchar_type( '!', symbol).
 pchar_type( '@', symbol).
 pchar_type( '#', symbol).
 pchar_type( '$', symbol).
 pchar_type( '%', symbol).
 pchar_type( '^', symbol).
 pchar_type( '&', symbol).
 pchar_type( '*', symbol).
 pchar_type( '(', symbol).
 pchar_type( ')', symbol).
 pchar_type( '[', symbol).
 pchar_type( ']', symbol).
 pchar_type( '{', symbol).
 pchar_type( '}', symbol).
 pchar_type( '|', symbol).
 pchar_type( '-', symbol).
 pchar_type( '_', symbol).
 pchar_type( '+', symbol).
 pchar_type( '=', symbol).
 pchar_type( '0', digit).
 pchar_type( '1', digit).
 pchar_type( '2', digit).
 pchar_type( '3', digit).
 pchar_type( '4', digit).
 pchar_type( '5', digit).
 pchar_type( '6', digit).
 pchar_type( '7', digit).
 pchar_type( '8', digit).
 pchar_type( '9', digit).
 pchar_type( ' ', whitespace).
 pchar_type( '\n', whitespace).
 pchar_type( '\r', whitespace).
 pchar_type( '\t', whitespace).
 
% phar_hex2(Char,Hex2). 
 pchar_hex2(a,hex2(6,1)).
 pchar_hex2(b,hex2(6,2)).
 pchar_hex2(c,hex2(6,3)).
 pchar_hex2(d,hex2(6,4)).
 pchar_hex2(e,hex2(6,5)).
 pchar_hex2(f,hex2(6,6)).
 pchar_hex2(g,hex2(6,7)).
 pchar_hex2(h,hex2(6,8)).
 pchar_hex2(i,hex2(6,9)).
 pchar_hex2(j,hex2(6,a)).
 pchar_hex2(k,hex2(6,b)).
 pchar_hex2(l,hex2(6,c)).
 pchar_hex2(m,hex2(6,d)).
 pchar_hex2(n,hex2(6,e)).
 pchar_hex2(o,hex2(6,f)).
 pchar_hex2(p,hex2(7,0)).
 pchar_hex2(q,hex2(7,1)).
 pchar_hex2(r,hex2(7,2)).
 pchar_hex2(s,hex2(7,3)).
 pchar_hex2(t,hex2(7,4)).
 pchar_hex2(u,hex2(7,5)).
 pchar_hex2(v,hex2(7,6)).
 pchar_hex2(w,hex2(7,7)).
 pchar_hex2(x,hex2(7,8)).
 pchar_hex2(y,hex2(7,9)).
 pchar_hex2(z,hex2(7,a)).
 pchar_hex2('A',hex2(4,1)).
 pchar_hex2('B',hex2(4,2)).
 pchar_hex2('C',hex2(4,3)).
 pchar_hex2('D',hex2(4,4)).
 pchar_hex2('E',hex2(4,5)).
 pchar_hex2('F',hex2(4,6)).
 pchar_hex2('G',hex2(4,7)).
 pchar_hex2('H',hex2(4,8)).
 pchar_hex2('I',hex2(4,9)).
 pchar_hex2('J',hex2(4,a)).
 pchar_hex2('K',hex2(4,b)).
 pchar_hex2('L',hex2(4,c)).
 pchar_hex2('M',hex2(4,d)).
 pchar_hex2('N',hex2(4,e)).
 pchar_hex2('O',hex2(4,f)).
 pchar_hex2('P',hex2(5,0)).
 pchar_hex2('Q',hex2(5,1)).
 pchar_hex2('R',hex2(5,2)).
 pchar_hex2('S',hex2(5,3)).
 pchar_hex2('T',hex2(5,4)).
 pchar_hex2('U',hex2(5,5)).
 pchar_hex2('V',hex2(5,6)).
 pchar_hex2('W',hex2(5,7)).
 pchar_hex2('X',hex2(5,8)).
 pchar_hex2('Y',hex2(5,9)).
 pchar_hex2('Z',hex2(5,a)).
 pchar_hex2(;,hex2(3,b)).
 pchar_hex2(:,hex2(3,a)).
 pchar_hex2('"',hex2(2,2)).
 pchar_hex2('\'',hex2(2,7)).
 pchar_hex2(?,hex2(3,f)).
 pchar_hex2(/,hex2(2,f)).
 pchar_hex2('\\',hex2(5,c)).
 pchar_hex2(<,hex2(3,c)).
 pchar_hex2(>,hex2(3,e)).
 pchar_hex2(',',hex2(2,c)).
 pchar_hex2('.',hex2(2,e)).
 pchar_hex2('!',hex2(2,1)).
 pchar_hex2(@,hex2(4,0)).
 pchar_hex2(#,hex2(2,3)).
 pchar_hex2($,hex2(2,4)).
 pchar_hex2('%',hex2(2,5)).
 pchar_hex2(^,hex2(5,e)).
 pchar_hex2(&,hex2(2,6)).
 pchar_hex2(*,hex2(2,a)).
 pchar_hex2('(',hex2(2,8)).
 pchar_hex2(')',hex2(2,9)).
 pchar_hex2('[',hex2(5,b)).
 pchar_hex2(']',hex2(5,d)).
 pchar_hex2('{',hex2(7,b)).
 pchar_hex2('}',hex2(7,d)).
 pchar_hex2('|',hex2(7,c)).
 pchar_hex2('-',hex2(2,d)).
 pchar_hex2('_',hex2(5,f)).
 pchar_hex2(+,hex2(2,b)).
 pchar_hex2(=,hex2(3,d)).
 pchar_hex2('0',hex2(3,0)).
 pchar_hex2('1',hex2(3,1)).
 pchar_hex2('2',hex2(3,2)).
 pchar_hex2('3',hex2(3,3)).
 pchar_hex2('4',hex2(3,4)).
 pchar_hex2('5',hex2(3,5)).
 pchar_hex2('6',hex2(3,6)).
 pchar_hex2('7',hex2(3,7)).
 pchar_hex2('8',hex2(3,8)).
 pchar_hex2('9',hex2(3,9)).
 pchar_hex2(' ',hex2(2,0)).
 pchar_hex2('\n',hex2(0,a)).
 pchar_hex2('\r',hex2(0,d)).
 pchar_hex2('\t',hex2(0,9)).
 
% pchar_code(Char,Code).
 pchar_code(a,97).
 pchar_code(b,98).
 pchar_code(c,99).
 pchar_code(d,100).
 pchar_code(e,101).
 pchar_code(f,102).
 pchar_code(g,103).
 pchar_code(h,104).
 pchar_code(i,105).
 pchar_code(j,106).
 pchar_code(k,107).
 pchar_code(l,108).
 pchar_code(m,109).
 pchar_code(n,110).
 pchar_code(o,111).
 pchar_code(p,112).
 pchar_code(q,113).
 pchar_code(r,114).
 pchar_code(s,115).
 pchar_code(t,116).
 pchar_code(u,117).
 pchar_code(v,118).
 pchar_code(w,119).
 pchar_code(x,120).
 pchar_code(y,121).
 pchar_code(z,122).
 pchar_code('A',65).
 pchar_code('B',66).
 pchar_code('C',67).
 pchar_code('D',68).
 pchar_code('E',69).
 pchar_code('F',70).
 pchar_code('G',71).
 pchar_code('H',72).
 pchar_code('I',73).
 pchar_code('J',74).
 pchar_code('K',75).
 pchar_code('L',76).
 pchar_code('M',77).
 pchar_code('N',78).
 pchar_code('O',79).
 pchar_code('P',80).
 pchar_code('Q',81).
 pchar_code('R',82).
 pchar_code('S',83).
 pchar_code('T',84).
 pchar_code('U',85).
 pchar_code('V',86).
 pchar_code('W',87).
 pchar_code('X',88).
 pchar_code('Y',89).
 pchar_code('Z',90).
 pchar_code(;,59).
 pchar_code(:,58).
 pchar_code('"',34).
 pchar_code('\'',39).
 pchar_code(?,63).
 pchar_code(/,47).
 pchar_code(\,92).
 pchar_code(<,60).
 pchar_code(>,62).
 pchar_code(',',44).
 pchar_code('.',46).
 pchar_code(!,33).
 pchar_code(@,64).
 pchar_code(#,35).
 pchar_code($,36).
 pchar_code('%',37).
 pchar_code(^,94).
 pchar_code(&,38).
 pchar_code(*,42).
 pchar_code('(',40).
 pchar_code(')',41).
 pchar_code('[',91).
 pchar_code(']',93).
 pchar_code('{',123).
 pchar_code('}',125).
 pchar_code('|',124).
 pchar_code(-,45).
 pchar_code('_',95).
 pchar_code(+,43).
 pchar_code(=,61).
 pchar_code('0',48).
 pchar_code('1',49).
 pchar_code('2',50).
 pchar_code('3',51).
 pchar_code('4',52).
 pchar_code('5',53).
 pchar_code('6',54).
 pchar_code('7',55).
 pchar_code('8',56).
 pchar_code('9',57).
 pchar_code(' ',32).
 pchar_code('\n',10).
 pchar_code('\r',13).
 pchar_code('\t',9).
 


/*
    HEX
*/

% hex_compare(Hex1,Hex2,Comparison).
hex_compare(hex(A), hex(B), C) :-
    hex_map(A, B, C).

% hex2_compare(Hex1,Hex2,Comparison).
hex2_compare(hex2(A1,B1), hex2(A2,B2), C) :-
    hex_map(A1, A2, Ch1),
    hex2_cmp(Ch1, B1, B2, C).

hex2_cmp(=, B1, B2, C) :- 
    hex_map(B1, B2, C).
hex2_cmp(>, _, _, >).
hex2_cmp(<, _, _, <).

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
