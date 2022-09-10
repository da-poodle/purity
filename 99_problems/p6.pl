% P6 - Find out whether a list is a palindrome.
% 
% Example:
%    palindrome([w,o,w]).
% true.
%
palindrome(N) :-
    preverse(N, N).

preverse(Xs,Rs) :- reverse_dl(Xs,Rs-[]).

reverse_dl([],T-T).
reverse_dl([X|Xs],Rs-T) :- reverse_dl(Xs,Rs-[X|T]).

words([
    [t,a,t,t,a,r,r,a,t,t,a,t],
    [c,o,u,n,t,r,y],
    [w,o,w],
    [f,a,c,t,o,r,y],
    [r,o,t,a,t,o,r],
    [c,r,a,z,y]
]).

test_palindrome(Word) :-
    words(W), 
    member(Word, W), 
    palindrome(Word).