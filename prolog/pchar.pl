:- module(pchar, [
   pchar_compare/3,
   pchar_upper/2,
   pchar_lower/2,
   pchar_type/2,
   pchar_code/2,
   pchar_hex2/2
]).

:- use_module(hex2).

pchar_compare(A, B, C) :-
   pchar_hex2(A, Ah),
   pchar_hex2(B, Bh),
   hex2_compare(Ah,Bh,C).

% gen_write :-
%     gen(Term),
%     writeln(Term),
%     fail.

% gen(Term) :- 
%     purity:domain(pchar, A, _), 
%     char_code(A, C), 
%     hex_bytes(H, [C]), 
%     atom_chars(H,[H1,H2]),
%     map_num(H1, H1x),
%     map_num(H2, H2x),
%     Term = pchar_hex2(A, hex2(H1x,H2x)). 

% map_num('0', 0).
% map_num('1', 1).
% map_num('2', 2).
% map_num('3', 3).
% map_num('4', 4).
% map_num('5', 5).
% map_num('6', 6).
% map_num('7', 7).
% map_num('8', 8).
% map_num('9', 9).
% map_num(A, A) :- char_type(A, alpha).



% Char is a character in the pchar domain.


% pchar_upper(Char, UpperChar).
%
% UpperChar is the uppercase version of Char
%
pchar_upper(C, U) :- ch_map(C, _, U).

% pchar_lower(Char, LowerChar).
% 
% LowerChar is the lowercase version of Char
%
pchar_lower(C, L) :- ch_map(C, L, _).

% ch_map(Char, Lower, Upper).
%
% map upper and lower case characters
%
ch_map( a, a, 'A').
ch_map( b, b, 'B').
ch_map( c, c, 'C').
ch_map( d, d, 'D').
ch_map( e, e, 'E').
ch_map( f, f, 'F').
ch_map( g, g, 'G').
ch_map( h, h, 'H').
ch_map( i, i, 'I').
ch_map( j, j, 'J').
ch_map( k, k, 'K').
ch_map( l, l, 'L').
ch_map( m, m, 'M').
ch_map( n, n, 'N').
ch_map( o, o, 'O').
ch_map( p, p, 'P').
ch_map( q, q, 'Q').
ch_map( r, r, 'R').
ch_map( s, s, 'S').
ch_map( t, t, 'T').
ch_map( u, u, 'U').
ch_map( v, v, 'V').
ch_map( w, w, 'W').
ch_map( x, x, 'X').
ch_map( y, y, 'Y').
ch_map( z, z, 'Z').
ch_map( 'A', a, 'A').
ch_map( 'B', b, 'B').
ch_map( 'C', c, 'C').
ch_map( 'D', d, 'D').
ch_map( 'E', e, 'E').
ch_map( 'F', f, 'F').
ch_map( 'G', g, 'G').
ch_map( 'H', h, 'H').
ch_map( 'I', i, 'I').
ch_map( 'J', j, 'J').
ch_map( 'K', k, 'K').
ch_map( 'L', l, 'L').
ch_map( 'M', m, 'M').
ch_map( 'N', n, 'N').
ch_map( 'O', o, 'O').
ch_map( 'P', p, 'P').
ch_map( 'Q', q, 'Q').
ch_map( 'R', r, 'R').
ch_map( 'S', s, 'S').
ch_map( 'T', t, 'T').
ch_map( 'U', u, 'U').
ch_map( 'V', v, 'V').
ch_map( 'W', w, 'W').
ch_map( 'X', x, 'X').
ch_map( 'Y', y, 'Y').
ch_map( 'Z', z, 'Z').
ch_map( ';', ';', ';').
ch_map( ':', ':', ':').
ch_map( '"', '"', '"').
ch_map( '\'','\'','\'').
ch_map( '\'','\'','\'').
ch_map( '/','/','/').
ch_map( '\\','\\','\\').
ch_map( '<','<','<').
ch_map( '>','>','>').
ch_map( ',',',',',').
ch_map( '.','.','.').
ch_map( '!','!','!').
ch_map( '@','@','@').
ch_map( '#','#','#').
ch_map( '$','$','$').
ch_map( '%','%','%').
ch_map( '^','^','^').
ch_map( '&','&','&').
ch_map( '*','*','*').
ch_map( '(','(','(').
ch_map( ')',')',')').
ch_map( '[','[','[').
ch_map( ']',']',']').
ch_map( '{','{','{').
ch_map( '}','}','}').
ch_map( '|','|','|').
ch_map( '-','-','-').
ch_map( '_','_','_').
ch_map( '+','+','+').
ch_map( '=','=','=').
ch_map( '0','0','0').
ch_map( '1','1','1').
ch_map( '2','2','2').
ch_map( '3','3','3').
ch_map( '4','4','4').
ch_map( '5','5','5').
ch_map( '6','6','6').
ch_map( '7','7','7').
ch_map( '8','8','8').
ch_map( '9','9','9').
ch_map( ' ',' ',' ').
ch_map( '\n','\n','\n').
ch_map( '\r','\r','\r').
ch_map( '\t','\t','\t').

% pchar_type(Char, Type).
%
% Type is the major type Char
% Type is one of alpha, digit, symbol, or whitespace
% 
pchar_type( a, alpha).
pchar_type( b, alpha).
pchar_type( c, alpha).
pchar_type( d, alpha).
pchar_type( e, alpha).
pchar_type( f, alpha).
pchar_type( g, alpha).
pchar_type( h, alpha).
pchar_type( i, alpha).
pchar_type( j, alpha).
pchar_type( k, alpha).
pchar_type( l, alpha).
pchar_type( m, alpha).
pchar_type( n, alpha).
pchar_type( o, alpha).
pchar_type( p, alpha).
pchar_type( q, alpha).
pchar_type( r, alpha).
pchar_type( s, alpha).
pchar_type( t, alpha).
pchar_type( u, alpha).
pchar_type( v, alpha).
pchar_type( w, alpha).
pchar_type( x, alpha).
pchar_type( y, alpha).
pchar_type( z, alpha).
pchar_type( 'A', alpha).
pchar_type( 'B', alpha).
pchar_type( 'C', alpha).
pchar_type( 'D', alpha).
pchar_type( 'E', alpha).
pchar_type( 'F', alpha).
pchar_type( 'G', alpha).
pchar_type( 'H', alpha).
pchar_type( 'I', alpha).
pchar_type( 'J', alpha).
pchar_type( 'K', alpha).
pchar_type( 'L', alpha).
pchar_type( 'M', alpha).
pchar_type( 'N', alpha).
pchar_type( 'O', alpha).
pchar_type( 'P', alpha).
pchar_type( 'Q', alpha).
pchar_type( 'R', alpha).
pchar_type( 'S', alpha).
pchar_type( 'T', alpha).
pchar_type( 'U', alpha).
pchar_type( 'V', alpha).
pchar_type( 'W', alpha).
pchar_type( 'X', alpha).
pchar_type( 'Y', alpha).
pchar_type( 'Z', alpha).
pchar_type( ';', symbol).
pchar_type( ':', symbol).
pchar_type( '"', symbol).
pchar_type( '\'', symbol).
pchar_type( '\'', symbol).
pchar_type( '/', symbol).
pchar_type( '\\', symbol).
pchar_type( '<', symbol).
pchar_type( '>', symbol).
pchar_type( ',', symbol).
pchar_type( '.', symbol).
pchar_type( '!', symbol).
pchar_type( '@', symbol).
pchar_type( '#', symbol).
pchar_type( '$', symbol).
pchar_type( '%', symbol).
pchar_type( '^', symbol).
pchar_type( '&', symbol).
pchar_type( '*', symbol).
pchar_type( '(', symbol).
pchar_type( ')', symbol).
pchar_type( '[', symbol).
pchar_type( ']', symbol).
pchar_type( '{', symbol).
pchar_type( '}', symbol).
pchar_type( '|', symbol).
pchar_type( '-', symbol).
pchar_type( '_', symbol).
pchar_type( '+', symbol).
pchar_type( '=', symbol).
pchar_type( '0', digit).
pchar_type( '1', digit).
pchar_type( '2', digit).
pchar_type( '3', digit).
pchar_type( '4', digit).
pchar_type( '5', digit).
pchar_type( '6', digit).
pchar_type( '7', digit).
pchar_type( '8', digit).
pchar_type( '9', digit).
pchar_type( ' ', whitespace).
pchar_type( '\n', whitespace).
pchar_type( '\r', whitespace).
pchar_type( '\t', whitespace).


pchar_hex2(a,hex2(6,1)).
pchar_hex2(b,hex2(6,2)).
pchar_hex2(c,hex2(6,3)).
pchar_hex2(d,hex2(6,4)).
pchar_hex2(e,hex2(6,5)).
pchar_hex2(f,hex2(6,6)).
pchar_hex2(g,hex2(6,7)).
pchar_hex2(h,hex2(6,8)).
pchar_hex2(i,hex2(6,9)).
pchar_hex2(j,hex2(6,a)).
pchar_hex2(k,hex2(6,b)).
pchar_hex2(l,hex2(6,c)).
pchar_hex2(m,hex2(6,d)).
pchar_hex2(n,hex2(6,e)).
pchar_hex2(o,hex2(6,f)).
pchar_hex2(p,hex2(7,0)).
pchar_hex2(q,hex2(7,1)).
pchar_hex2(r,hex2(7,2)).
pchar_hex2(s,hex2(7,3)).
pchar_hex2(t,hex2(7,4)).
pchar_hex2(u,hex2(7,5)).
pchar_hex2(v,hex2(7,6)).
pchar_hex2(w,hex2(7,7)).
pchar_hex2(x,hex2(7,8)).
pchar_hex2(y,hex2(7,9)).
pchar_hex2(z,hex2(7,a)).
pchar_hex2('A',hex2(4,1)).
pchar_hex2('B',hex2(4,2)).
pchar_hex2('C',hex2(4,3)).
pchar_hex2('D',hex2(4,4)).
pchar_hex2('E',hex2(4,5)).
pchar_hex2('F',hex2(4,6)).
pchar_hex2('G',hex2(4,7)).
pchar_hex2('H',hex2(4,8)).
pchar_hex2('I',hex2(4,9)).
pchar_hex2('J',hex2(4,a)).
pchar_hex2('K',hex2(4,b)).
pchar_hex2('L',hex2(4,c)).
pchar_hex2('M',hex2(4,d)).
pchar_hex2('N',hex2(4,e)).
pchar_hex2('O',hex2(4,f)).
pchar_hex2('P',hex2(5,0)).
pchar_hex2('Q',hex2(5,1)).
pchar_hex2('R',hex2(5,2)).
pchar_hex2('S',hex2(5,3)).
pchar_hex2('T',hex2(5,4)).
pchar_hex2('U',hex2(5,5)).
pchar_hex2('V',hex2(5,6)).
pchar_hex2('W',hex2(5,7)).
pchar_hex2('X',hex2(5,8)).
pchar_hex2('Y',hex2(5,9)).
pchar_hex2('Z',hex2(5,a)).
pchar_hex2(;,hex2(3,b)).
pchar_hex2(:,hex2(3,a)).
pchar_hex2('"',hex2(2,2)).
pchar_hex2('\'',hex2(2,7)).
pchar_hex2(?,hex2(3,f)).
pchar_hex2(/,hex2(2,f)).
pchar_hex2('\\',hex2(5,c)).
pchar_hex2(<,hex2(3,c)).
pchar_hex2(>,hex2(3,e)).
pchar_hex2(',',hex2(2,c)).
pchar_hex2('.',hex2(2,e)).
pchar_hex2('!',hex2(2,1)).
pchar_hex2(@,hex2(4,0)).
pchar_hex2(#,hex2(2,3)).
pchar_hex2($,hex2(2,4)).
pchar_hex2('%',hex2(2,5)).
pchar_hex2(^,hex2(5,e)).
pchar_hex2(&,hex2(2,6)).
pchar_hex2(*,hex2(2,a)).
pchar_hex2('(',hex2(2,8)).
pchar_hex2(')',hex2(2,9)).
pchar_hex2('[',hex2(5,b)).
pchar_hex2(']',hex2(5,d)).
pchar_hex2('{',hex2(7,b)).
pchar_hex2('}',hex2(7,d)).
pchar_hex2('|',hex2(7,c)).
pchar_hex2('-',hex2(2,d)).
pchar_hex2('_',hex2(5,f)).
pchar_hex2(+,hex2(2,b)).
pchar_hex2(=,hex2(3,d)).
pchar_hex2('0',hex2(3,0)).
pchar_hex2('1',hex2(3,1)).
pchar_hex2('2',hex2(3,2)).
pchar_hex2('3',hex2(3,3)).
pchar_hex2('4',hex2(3,4)).
pchar_hex2('5',hex2(3,5)).
pchar_hex2('6',hex2(3,6)).
pchar_hex2('7',hex2(3,7)).
pchar_hex2('8',hex2(3,8)).
pchar_hex2('9',hex2(3,9)).
pchar_hex2(' ',hex2(2,0)).
pchar_hex2('\n',hex2(0,a)).
pchar_hex2('\r',hex2(0,d)).
pchar_hex2('\t',hex2(0,9)).

% -- GENERATED : Character Codes --

pchar_code(a,97).
pchar_code(b,98).
pchar_code(c,99).
pchar_code(d,100).
pchar_code(e,101).
pchar_code(f,102).
pchar_code(g,103).
pchar_code(h,104).
pchar_code(i,105).
pchar_code(j,106).
pchar_code(k,107).
pchar_code(l,108).
pchar_code(m,109).
pchar_code(n,110).
pchar_code(o,111).
pchar_code(p,112).
pchar_code(q,113).
pchar_code(r,114).
pchar_code(s,115).
pchar_code(t,116).
pchar_code(u,117).
pchar_code(v,118).
pchar_code(w,119).
pchar_code(x,120).
pchar_code(y,121).
pchar_code(z,122).
pchar_code('A',65).
pchar_code('B',66).
pchar_code('C',67).
pchar_code('D',68).
pchar_code('E',69).
pchar_code('F',70).
pchar_code('G',71).
pchar_code('H',72).
pchar_code('I',73).
pchar_code('J',74).
pchar_code('K',75).
pchar_code('L',76).
pchar_code('M',77).
pchar_code('N',78).
pchar_code('O',79).
pchar_code('P',80).
pchar_code('Q',81).
pchar_code('R',82).
pchar_code('S',83).
pchar_code('T',84).
pchar_code('U',85).
pchar_code('V',86).
pchar_code('W',87).
pchar_code('X',88).
pchar_code('Y',89).
pchar_code('Z',90).
pchar_code(;,59).
pchar_code(:,58).
pchar_code('"',34).
pchar_code('\'',39).
pchar_code(?,63).
pchar_code(/,47).
pchar_code(\,92).
pchar_code(<,60).
pchar_code(>,62).
pchar_code(',',44).
pchar_code('.',46).
pchar_code(!,33).
pchar_code(@,64).
pchar_code(#,35).
pchar_code($,36).
pchar_code('%',37).
pchar_code(^,94).
pchar_code(&,38).
pchar_code(*,42).
pchar_code('(',40).
pchar_code(')',41).
pchar_code('[',91).
pchar_code(']',93).
pchar_code('{',123).
pchar_code('}',125).
pchar_code('|',124).
pchar_code(-,45).
pchar_code('_',95).
pchar_code(+,43).
pchar_code(=,61).
pchar_code('0',48).
pchar_code('1',49).
pchar_code('2',50).
pchar_code('3',51).
pchar_code('4',52).
pchar_code('5',53).
pchar_code('6',54).
pchar_code('7',55).
pchar_code('8',56).
pchar_code('9',57).
pchar_code(' ',32).
pchar_code('\n',10).
pchar_code('\r',13).
pchar_code('\t',9).
