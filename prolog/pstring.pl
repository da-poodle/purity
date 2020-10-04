
:- module(pstring, [
        
    pchar/1,
    pchar_upper/2,
    pchar_lower/2,
    pchar_type/2,
    pchar_code/2,

    % String Library
    pstr_empty/1,
    pstr_upper/2,
    pstr_lower/2,
    pstr_split/3,
    pstr_join/3,
    pstr_contains/2,
    pstr_contains/3,
    pstr_prefix/2,
    pstr_prefix/3,
    pstr_trim/2,
    pstr_replace/4
]).

:- use_module(purity).
:- use_module(pchar).

:- multifile purity:pcompare/4.

purity:pcompare(pstring, A, B, C) :- pcompare(plist(pchar), A, B, C).
purity:pcompare(pchar, A, B, C) :- ch(A, B, C). 


% pstr_empty(Str).
%
% Str is an empty string
%
pstr_empty([]).


% pstr_upper(String, Upper).
%
% Upper is the upper case version of String
%
% C = the first character of String
% T = the tail of Sting
% U = the upper case version of C
% R = the tail of Upper
%
pstr_upper([], []).
pstr_upper([C|T], [U|R]) :-
        ch_map(C, _, U),
        pstr_upper(T, R).


% pstr_lower(String, Lower).
%
% Lower is the lower case version of String
%
% C = the first character of String
% T = the tail of Sting
% U = the lower case version of C
% R = the tail of Lower
%
pstr_lower([], []).
pstr_lower([C|T], [L|R]) :-
        ch_map(C, L, _),
        pstr_lower(T, R).


% pstr_replace(String, Find, Replace, Replaced).
%
% Replaced is String with the first instance of Find changed to Replace
%
% A = the first character of String
% T = the tail of String
% F = Find
% R = Replace
% Rp = Replaced
% Rpt = the tail of Replaced
% Orig = String or part of String
% 
pstr_replace([], _, _, []).
pstr_replace([A|T], F, R, Rp) :-
        pstr_prefix(F, [A|T], IsPrefix),
        pstr_replace_(IsPrefix, [A|T], F, R, Rp).

pstr_replace_(true, Orig, F, R, Rp) :-
        append(F, Remaining, Orig),
        append(R, Remaining, Rp).
pstr_replace_(false, [A|T], F, R, [A|Rpt]) :-
        pstr_replace(T, F, R, Rpt).


% pstr_trim(UnTrimmed, Trimmed).
%
% Trimmed is UnTrimmed with all whitespace removed from the start and end
%
% A = the first character of UnTrimmed
% T = the tail of UnTrimmed
% Out = Trimmed
% R = true of false based on whether the end of UnTrimmed is all whitespace or not
%
pstr_trim([A|T], Out) :-
        pchar_type(A, Type),
        pstr_trim_start(Type, A, T, Out).

pstr_trim_start(whitespace, _, T, Out) :-  pstr_trim(T, Out).
pstr_trim_start(alpha, A, T, [A|Out]) :- pstr_trim_end(T, Out).
pstr_trim_start(digit, A, T, [A|Out]) :- pstr_trim_end(T, Out).
pstr_trim_start(symbol, A, T, [A|Out]) :- pstr_trim_end(T, Out).

pstr_trim_end([], []).
pstr_trim_end([A|T], Trimmed) :-
        pstr_all_whitespace([A|T], R),
        pstr_trim_end_(R, A, T, Trimmed).

pstr_trim_end_(true, _, _, []).
pstr_trim_end_(false, A, T, [A|End]) :- pstr_trim_end(T, End).

pstr_all_whitespace([], true).
pstr_all_whitespace([A|T], R) :-
        pchar_type(A, Type),
        pstr_all_whitespace_(Type, T, R).

pstr_all_whitespace_(whitespace, T, R) :- pstr_all_whitespace(T, R).
pstr_all_whitespace_(alpha, _, false).
pstr_all_whitespace_(digit, _, false).
pstr_all_whitespace_(symbol, _, false).

% pstr_split(StrToSplit, DelimiterChar, Split).
% 
% Split is StrToSplit separated by DelimiterChar into lists. 
% DelimiterChar is not included in Split.
%
pstr_split(A, D, B) :-
        pchar(D),
        pstr_split_(A, D, B).

pstr_split_([], _, []).
pstr_split_([A|T], D, Split) :-
        pdif_char(A,D,B),
        pstr_split(B,A,T,D,Split).

pstr_split(false, D, T, D, Split) :-
        pstr_split_(T,D,Split).
pstr_split(true, A, T, D, [S|Rest]) :-
        find_split([A|T], D, Rem, S),
        pstr_split_(Rem, D, Rest). 

find_split([], _, [], []).
find_split([A|T], D, Rem, Split) :-
        pdif_char(A,D,B),
        find_split(B, A, T, D, Rem, Split).

find_split(false, D, T, D, T, []).
find_split(true, A, T, D, Rem, [A|Rest]) :-
        find_split(T, D, Rem, Rest).



% pstr_join(ListOfStrings, Delimiter, Joined).
%
% Joined is ListOfStrings flattened into a single list and separated by Delimiter
%
pstr_join(A, D, B) :-
        pchar(D),
        pstr_join_(A, D, B).

pstr_join_([], _, []).
pstr_join_([A|T], D, S) :-
        pstr_join(A, T, D, S).

pstr_join([], T, D, R) :-
        pstr_join2(T, D, R).
pstr_join([A|At], T, D, [A|R]) :-
        pstr_join(At, T, D, R).

pstr_join2([], _, []).
pstr_join2([N|T], D, [D|R]) :-
        pstr_join_([N|T], D, R).


% pstr_contains(String, SubString).
%
% Holds if SubString is a sequence within String
%
pstr_contains(String, SubString) :-
         pstr_contains(String, SubString, true).

% pstr_contains(String, SubString, Contains).
% 
% Contains is true if SubString is a sequence within String, otherwise false
%
pstr_contains([], _, false). 
pstr_contains([A|T], B, C) :-
        pstr_prefix(B, [A|T], Prefix),
        pstr_contains_(Prefix, [A|T], B, C).

pstr_contains_(true, _, _, true).
pstr_contains_(false, [_|At], B, C) :-
        pstr_contains(At, B, C).


% pstr_prefix(Prefix, String).
%
% Holds if Prefix is the starting sequence of String
%
pstr_prefix(Prefix, String) :-
        pstr_prefix(Prefix, String, true).

% pstr_prefix(Prefix, String, IsPrefix).
%
% IsPrefix is true if Prefix is the starting sequence of String
%
pstr_prefix([], _, true).
pstr_prefix([A|At], [B|Bt], IsPrefix) :-
        ch(A, B, C),
        pstr_prefix(C, At, Bt, IsPrefix).

pstr_prefix(=, At, Bt, B) :- 
        pstr_prefix(At, Bt, B).
pstr_prefix(<, _, _, false).
pstr_prefix(>, _, _, false).
