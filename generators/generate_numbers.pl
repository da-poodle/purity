generate :- 
    between(0, 9, A), 
    between(0, 9, B), 
    N is A + B, 
    number_chars(N, Nc),
    out_term(Nc,A,B).

out_term([N],A,B) :-
    convert(A,Ac),
    convert(B,Bc),
    write_term(user_output, sn_sum(Ac, Bc, N, '0'), [nl(true), fullstop(true)]).
out_term([1,N],A,B) :-
    convert(A,Ac),
    convert(B,Bc),
    write_term(user_output, sn_sum(Ac, Bc, N, '1'), [nl(true), fullstop(true)]).

convert(0,'0').
convert(1,'1').
convert(2,'2').
convert(3,'3').
convert(4,'4').
convert(5,'5').
convert(6,'6').
convert(7,'7').
convert(8,'8').
convert(9,'9').