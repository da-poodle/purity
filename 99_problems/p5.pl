% P05 - Reverse a list.
%
% Example:
%    preverse([a,b,c,d], [d,c,b,a]).
preverse(L1,L2) :- prev(L1,L2,[]).

prev([],L2,L2).
prev([X|Xs],L2,Acc) :- 
    prev(Xs,L2,[X|Acc]).
