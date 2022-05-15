
istree(l(_)).
istree(tl(_,L)) :- 
    istree(L).
istree(tr(_,R)) :-
    istree(R).
istree(t(_,L,R)) :-
    istree(L),
    istree(R).
