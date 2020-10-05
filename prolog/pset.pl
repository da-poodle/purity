:- module(pset, [
    pset_empty/2,
    list_set/3,
    set_list/2,
    subset/3,
    punion/3,
    pdifference/3,
    pintersection/3
]).

:- use_module(purity).


% p_is_set/1
pset_empty(Domain, set(Domain, [])).

% subset(SubSet, Set, Truth)
%
% Truth is true if Set contains SubSet, otherwise false.
%
subset(set(D, Sub), set(D, Set), T) :-
    subset_(Sub, Set, D, T).

subset_([],_,_,true).
subset_([A|Sub],Set,D,T) :-
    subset__(Set, [A|Sub],D,T).

subset__([],_,_,false).
subset__([B|Set],[A|Sub],D,T) :-
    pcompare(D,A,B,C),
    subset_c(C,[A|Sub],[B|Set],D,T).

subset_c(=,[_|Sub],[_|Set],D,T) :- 
    subset_(Sub,Set,D,T).
subset_c(<,_,_,_,false).
subset_c(>,Sub,[_|Set],D,T) :- 
    subset_(Sub,Set,D,T).

% list_set(Domain, List, Set).

list_set(D, L, set(D, Set)) :-
    psort(D,L,B),
    remove_dups_sorted(B,D,Set).

remove_dups_sorted([], _, []).
remove_dups_sorted([A|T],D,R) :-
    remove_dups_sorted_(T,A,D,R).

remove_dups_sorted_([],A,_,[A]).
remove_dups_sorted_([B|T],A,D,R) :-
    pdif(D,A,B,C),
    remove_dups_sorted_(C,[A,B|T],D,R).

remove_dups_sorted_(true,[A,B|T],D,[A|R]) :-
    remove_dups_sorted([B|T],D,R).
remove_dups_sorted_(false,[A,A|T],D,R) :-
    remove_dups_sorted([A|T],D,R).


set_list(set(_, List), List).


% punion/3
punion(set(D,S1),set(D,S2),set(D,S3)) :-
    punion_(S1,S2,S3,D).
 
punion_([],B,B,_).
punion_([A|At],B,R,D) :-
    punion_1(B,[A|At],R,D).

punion_1([],A,A,_).
punion_1([B|Bt],[A|At],R,D) :-
    pcompare(D,A,B,C),
    punion_c(C,[A|At],[B|Bt],R,D).

punion_c(=,[A|At],[A|Bt],[A|R],D) :- 
    punion_(At,Bt,R,D).
punion_c(<,[A|At],[B|Bt],[A|R],D) :- 
    punion_(At,[B|Bt],R,D).
punion_c(>,[A|At],[B|Bt],[B|R],D) :- 
    punion_([A|At],Bt,R,D).


% difference(Set1, Set2, Set3).
%
% Set3 is Set1 - Set2
%
pdifference(set(D,S1),set(D,S2),set(D,S3)) :-
    pdifference_(S1,S2,S3,D).
 
pdifference_([],_,[],_).
pdifference_([A|At],B,R,D) :-
    pdifference_1(B,[A|At],R,D).

pdifference_1([],A,A,_).
pdifference_1([B|Bt],[A|At],R,D) :-
    pcompare(D,A,B,C),
    pdifference_c(C,[A|At],[B|Bt],R,D).

pdifference_c(=,[A|At],[A|Bt],R,D) :- 
    pdifference_(At,Bt,R,D).
pdifference_c(<,[A|At],[B|Bt],[A|R],D) :- 
    pdifference_(At,[B|Bt],R,D).
pdifference_c(>,[A|At],[_|Bt],[A|R],D) :- 
    pdifference_([A|At],Bt,R,D).


% pintersection/3
pintersection(set(D,S1),set(D,S2),set(D,S3)) :-
    pintersection_(S1,S2,S3,D).
 
pintersection_([],_,[],_).
pintersection_([A|At],B,R,D) :-
    pintersection_1(B,[A|At],R,D).

pintersection_1([],_,[],_).
pintersection_1([B|Bt],[A|At],R,D) :-
    pcompare(D,A,B,C),
    pintersection_c(C,[A|At],[B|Bt],R,D).

pintersection_c(=,[A|At],[A|Bt],[A|R],D) :- 
    pintersection_(At,Bt,R,D).
pintersection_c(<,[_|At],[B|Bt],R,D) :- 
    pintersection_(At,[B|Bt],R,D).
pintersection_c(>,[A|At],[_|Bt],R,D) :- 
    pintersection_([A|At],Bt,R,D).
