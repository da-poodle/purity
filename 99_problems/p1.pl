% P01 - Find the last element of a list. 
%
% Example:
%     last([a,b,c,d], d).
%
last([A],A).
last([_|T],A) :-
    last(T,A).
