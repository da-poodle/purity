:- use_module(plist).
:- use_module(pset).

% element_of(Element, Collection).
element_of(E, [A|T]) :- member(E, [A|T]).
element_of(E, set(_, S)) :- member(E, S).


% element_in(Element, Collection).
element_in(E, [A|T]) :- pmemberchk(_, E, [A|T]).
element_in(E, set(D, S)) :- pmemberchk(D, E, S).


% apply_from_to(Goal, Source, Mapped)
apply_list_([], [], _).
apply_list_([Elem1|Tail1], [Elem2|Tail2], Goal) :-
    call(Goal, Elem1, Elem2),
    apply_list_(Tail1, Tail2, Goal).

apply_general([], [], _).
apply_general([A|T], Mapped, Goal) :- apply_list_([A|T], Mapped, Goal).
apply_general(set(D, S), Set, Goal) :- 
    apply_list_(S, Mapped, Goal),
    list_set(D, Mapped, Set).

apply_from_to(Goal, Source, Mapped) :-
    apply_general(Source, Mapped, Goal).


% apply_all(Goal, Source).
apply_all_list_([], _).
apply_all_list_([A|T], Goal) :-
    call(Goal, A),
    apply_all_list_(T, Goal).

apply_all_general([], _).
apply_all_general([A|T], Goal) :- apply_all_list_([A|T], Goal).
apply_all_general(set(_, Set), Goal) :- apply_all_list_(Set, Goal).

apply_all(Goal, Collection) :- apply_all_general(Collection, Goal).


% apply_any(Goal, Source).
apply_any_list_([A|T], Goal) :- 
    call(Goal, A, R),
    apply_any_list_c(R, T, Goal).

apply_any_list_c(true, _, _).
apply_any_list_c(false, L, Goal) :-
    apply_any_list_(L, Goal).

apply_any_general([], _).
apply_any_general([A|T], Goal) :- apply_any_list_([A|T], Goal).
apply_any_general(set(_, Set), Goal) :- apply_any_list_(Set, Goal).

apply_any(Goal, Collection) :- apply_any_general(Collection, Goal).


% reduce_from_start_to(Goal, Collection, Start, Result).


% filter_from_to(Goal, Source, Filtered).


% filter_from_to_rem(Goal, Source, Included, Excluded).


% collection_size(Collection, Size).


% element_with_without(Element, With, Without).

