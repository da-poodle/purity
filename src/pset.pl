:- ensure_loaded(purity).
:- ensure_loaded(plist).

:- multifile(pcompare/4).
:- multifile(ptype/2).

ptype(pset([]), pset).
ptype(pset([_,_]), pset).

pcompare(pset, pset(A), pset(B), C) :-
    % compare list contents
    pcompare(A, B, C).

% p_is_set/1
pset_empty(pset([])).

% subset_t(SubSet, Set, Truth)
subset_t(pset(Sub), pset(Set), T) :-
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
list_set(L, pset(Set)) :-
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
set_list(pset(List), List).


% punion(L1,L2,Union).
punion(pset(S1), pset(S2), pset(S3)) :-
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
pdifference(pset(S1),pset(S2),pset(S3)) :-
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
pintersection(pset(S1),pset(S2),pset(S3)) :-
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

