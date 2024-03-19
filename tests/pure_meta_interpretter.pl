% Meta interpretter for 'pure prolog'
mi(Goal) :-
        \+ unclausable(Goal),
        clause(Goal, Body),
        mi(Body).
mi(true).
mi(fail) :- fail.
mi(','(A,B)) :-
        mi(A),
        mi(B).
mi('='(A,B)) :- A = B.
mi(';'(A,B)) :- 
        mi(A) 
        ; 
        mi(B).

unclausable(_ = _).
unclausable((_,_)).
unclausable((_;_)).
unclausable(true).
unclausable(fail).
    
