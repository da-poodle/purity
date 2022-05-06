% P18 (**) Extract a slice from a list.
%    Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
%
%    Example:
%    ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
%    X = [c,d,e,f,g]
slice_u([X|Xs],c(z),c(End),[X|Ys]) :- 
    slice_u(Xs,End,Ys).
slice_u([_|Xs],c(c(Start)),c(End),Ys) :-
    slice_u(Xs,c(Start),End,Ys).

slice_u([X|_],c(z),[X]).
slice_u([X|Xs],c(c(End)),[X|Ys]) :-  
    slice_u(Xs,c(End),Ys).

slice(Xs, Start, End, Ys) :-
    number_unary(Start, StartU),
    number_unary(End, EndU),
    slice_u(Xs, StartU, EndU, Ys).



number_unary(0, z).
number_unary(1, c(z)).
number_unary(2, c(c(z))).
number_unary(3, c(c(c(z)))).
number_unary(4, c(c(c(c(z))))).
number_unary(5, c(c(c(c(c(z)))))).
number_unary(6, c(c(c(c(c(c(z))))))).
number_unary(7, c(c(c(c(c(c(c(z)))))))).
number_unary(8, c(c(c(c(c(c(c(c(z))))))))).
number_unary(9, c(c(c(c(c(c(c(c(c(z)))))))))).
number_unary(10, c(c(c(c(c(c(c(c(c(c(z))))))))))).
number_unary(11, c(C)) :- number_unary(10, C).