% plist domain (plist)
%
% A domain that works over lists
%
:- module(domain_plist, []).

:- use_module(plist).

:- multifile purity:domain/3.
:- multifile purity:pcompare/3.

purity:domain(plist, A, B) :- 
    plist(A),
    maplist(domain(_), A, B).
purity:domain(plist(Domain), A, B) :-
    plist(A),
    maplist(domain(Domain), A, B).

purity:pcompare(A, B, C) :- 
    domain(plist(D), _, A),
    domain(plist(D), _, B),
    plist_compare(A, B, C, D). 

% plist_compare(List1, List2, Comparator, Domain).
%
% compare two lists of type 'Domain' 
% Comparator contains either =, < or >.
% comparisons are in relation to the first list
% eg: if List1 = 3 and List2 = 4 then List1 < List2
%
% D = domain
% C = the comparator operator
% A = list1 element to compare
% B = list2 element to compare
% T1 = tail of list1
% T2 = tail of list2
% S = list2
% TC = temporary compartor
%
plist_compare([],S,C, _) :- plist_compare_0(S, C).
plist_compare([A|T1], S, C, D) :- plist_compare_1(S, [A|T1], C, D).

plist_compare_0([], =).
plist_compare_0([_|_], <).

plist_compare_1([], _, >, _).
plist_compare_1([A|T1], [B|T2], C, D) :-
        pcompare(B, A, TC), 
        plist_compare_cond(TC, T2, T1, C, D).

plist_compare_cond(<, _, _, <, _).
plist_compare_cond(>, _, _, >, _).
plist_compare_cond(=, T1, T2, C, D) :- 
        plist_compare(T1, T2, C, D).