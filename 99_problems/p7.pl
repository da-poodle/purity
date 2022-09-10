% horn clause solution with lists
% 
% Example: 
%    flatten_hc([a, [b, [c, d], e]], Xs).
%
flatten_hc(X,[X]) :- 
    char(X). 
flatten_hc([],[]).
flatten_hc([X|Xs],Zs) :- 
    flatten_hc(X,Y), 
    flatten_hc(Xs,Ys), 
    pappend(Y,Ys,Zs).

char(a). 
char(b).
char(c).
char(d).
char(e).
char(f).
char(g).
char(h).
char(i).
char(j).
char(k).
char(l).
char(m).
char(n).
char(o).
char(p).
char(q).
char(r).
char(s).
char(t).
char(u).
char(v).
char(w).
char(x).
char(y).
char(z).

pappend([], A, A).
pappend([A|B], C, [A|D]) :-
    pappend(B, C, D).

