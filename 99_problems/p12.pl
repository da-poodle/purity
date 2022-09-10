% P12 (**) Decode a run-length encoded list.
%    Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
decode([], []).
decode([X|T], [X|D]) :- 
    char(X),
    decode(T, D).
decode([[c(zero),X]|T], [X|D]) :- 
    decode(T,D).
decode([[c(c(N)),X]|T], [X|D]) :- 
    decode([[c(N),X]|T], D).

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

