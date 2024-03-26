hexval(A) :- member(A, [0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f]).

hex(hex(A,B)) :-
    hexval(A),
    hexval(B).

generate :-
    findall(H, hex(H), Hs), 
    open('pcode.pl', append, S),
    forall(
        nth0(N, Hs, Hex), 
        write_term(S, code_hex2(N, Hex), [nl(true), fullstop(true)])),
    close(S).