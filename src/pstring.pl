% pstr_upper(String, Upper).
pstr_upper([], []).
pstr_upper([C|T], [U|R]) :-
    pchar:ch_map(C, _, U),
    pstr_upper(T, R).

% pstr_lower(String, Lower).
pstr_lower([], []).
pstr_lower([C|T], [L|R]) :-
    pchar:ch_map(C, L, _),
    pstr_lower(T, R).

% pstr_replace(String, Find, Replace, Replaced).
pstr_replace([], _, _, []).
pstr_replace([A|T], F, R, Rp) :-
    pstr_prefix_t(F, [A|T], IsPrefix),
    pstr_replace_(IsPrefix, [A|T], F, R, Rp).

pstr_replace_(true, Orig, F, R, Rp) :-
    append(F, Remaining, Orig),
    append(R, Remaining, Rp).
pstr_replace_(false, [A|T], F, R, [A|Rpt]) :-
    pstr_replace(T, F, R, Rpt).

% pstr_trim(UnTrimmed, Trimmed).
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
pstr_split(A, D, B) :-
    ptype(D, pchar),
    pstr_split_(A, D, B).

pstr_split_([], _, []).
pstr_split_([A|T], D, Split) :-
    pdif(A,D,B),
    pstr_split(B,A,T,D,Split).

pstr_split(false, D, T, D, Split) :-
    pstr_split_(T,D,Split).
pstr_split(true, A, T, D, [S|Rest]) :-
    find_split([A|T], D, Rem, S),
    pstr_split_(Rem, D, Rest). 

find_split([], _, [], []).
find_split([A|T], D, Rem, Split) :-
    pdif(A,D,B),
    find_split(B, A, T, D, Rem, Split).

find_split(false, D, T, D, T, []).
find_split(true, A, T, D, Rem, [A|Rest]) :-
    find_split(T, D, Rem, Rest).

% pstr_join(ListOfStrings, Delimiter, Joined).
pstr_join(A, D, B) :-
    ptype(D, pchar),
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
pstr_contains(String, SubString) :-
    pstr_contains_t(String, SubString, true).

% pstr_contains(String, SubString, Contains).
pstr_contains([], _, false). 
pstr_contains([A|T], B, C) :-
    pstr_prefix_t(B, [A|T], Prefix),
    pstr_contains_(Prefix, [A|T], B, C).

pstr_contains_(true, _, _, true).
pstr_contains_(false, [_|At], B, C) :-
    pstr_contains_t(At, B, C).

% pstr_prefix(Prefix, String).
pstr_prefix(Prefix, String) :-
    pstr_prefix_t(Prefix, String, true).

% pstr_prefix(Prefix, String, IsPrefix).
pstr_prefix_t([], _, true).
pstr_prefix_t([A|At], [B|Bt], IsPrefix) :-
    pcompare(A, B, C),
    pstr_prefix(C, At, Bt, IsPrefix).

pstr_prefix(=, At, Bt, B) :- 
    pstr_prefix_t(At, Bt, B).
pstr_prefix(<, _, _, false).
pstr_prefix(>, _, _, false).



