% Meta interpretter for 'pure prolog'
mi(Goal) :-
        \+ unclausable(Goal),
        clause(Goal, Body),
        mi(Body).
mi(true).
mi(','(A,B)) :-
        mi(A),
        mi(B).

unclausable((_,_)).
unclausable(true).
    
