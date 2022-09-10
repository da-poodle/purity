% P18 (**) Extract a slice from a list.
%    Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
%
%    Example:
%    ?- slice([a,b,c,d,e,f,g,h,i,k],c(c(c(zero))),c(c(c(c(c(c(c(zero))))))),L).
%    X = [c,d,e,f,g]
%
slice([X|Xs],c(zero),c(End),[X|Ys]) :- 
    slice(Xs,End,Ys).
slice([_|Xs],c(Start),c(End),Ys) :- 
    slice(Xs,Start,End,Ys).

slice([X|_],c(zero),[X]).
slice([X|Xs],c(End),[X|Ys]) :-  
    slice(Xs,End,Ys).

