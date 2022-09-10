% P02 - Find the last but one element of a list. 
%
% example
% last_but_one([a,b,c,d], c).
%
last_but_one([E,_], E).
last_but_one([_,B,C|T], E) :- 
    last_but_one([B,C|T], E).

