:- use_module('../prolog/purity.pl').
%:- use_module('../prolog/plist.pl').
:- use_module('../prolog/pchar.pl').

purity:pcompare(A, B, C) :- pchar_compare(A, B, C).

quicksort([],[]).
quicksort([X|Xs],Sorted) :-
  partition(X,Xs,Smalls,Bigs),
  quicksort(Smalls,SortedSmalls),
  quicksort(Bigs,SortedBigs),
  append(SortedSmalls,[X|SortedBigs],Sorted).

partition(_Pivot,[],[],[]).
partition(Pivot,[X|Xs],[X|Ys],Zs) :-
   lte(X, Pivot),
   partition(Pivot,Xs,Ys,Zs).
partition(Pivot,[X|Xs],Ys,[X|Zs]) :-
   gt(X, Pivot),
   partition(Pivot,Xs,Ys,Zs).


/* difference list version of quicksort */

quicksort2(L,S) :- quicksort_dl(L,S-[]).

quicksort_dl([],S-S).
quicksort_dl([X|Xs],Sorted-Tail) :-
  partition(X,Xs,Smalls,Bigs),
 /* the recursive calls splice the two lists together, with the split element
    in the middle */
  quicksort_dl(Smalls,Sorted-[X|T]),  
  quicksort_dl(Bigs,T-Tail).
