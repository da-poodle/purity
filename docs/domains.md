Up: [Purity](intro.md)

## Creating custom Domains

To use the Purity library a Domain needs to be imported or created. By default there are a few Domains that are available but they might not suit the purpose of the programmer.

To create a domain then the `purity:pcompare/4` predicate needs to be implemented with a few rules. Straying from these rules is possible but the logical purity of the program may be compromised.

1. A general call (using all variables) will generate domain values.
1. A value that is not in the Domain will create a fail result.
1. `purity:pcompare/4` will produce =, < or > as the last argument.

## Horn Clause only Domains

There are a few options that can be used to create usable Domains using only Horn Clauses. This is necessary if your prolog implementation only supports Horn Clauses, and interesting as well.

#### Option 1: before_in_list/3

By passing a list that represents the domain, before_in_list/3 will determine if a value is in fact less than another value according to the order of the list.

    before_in_list([A|T],A,B) :-
        member(B,T).
    before_in_list([_|T],A,B) :-
        before_in_list(T,A,B).

This works if the comparison list passed in contains a unique set of values.

To use, implement `purity:compare/4` as follows:

    domain_list([1,2,3,4,5,6,7,8,9]).

    pcompare(one_to_ten,A,A,=).
    pcompare(one_to_ten,A,B,<) :-
        domain_list(D),
        before_in_list(D,A,B).
    pcompare(one_to_ten,A,B,>) :-
        domain_list(D),
        before_in_list(D,B,A).

This technique is simple and covers the rules of a good Domain but will produce choice points. The performance over large domains is questionable at best.

#### Option 2: Successor

Using a successor is a good way to implement comparison as well for smaller data sets. For example, comparing numbers one to ten can be done as follows:

    psucc(1,2).
    psucc(2,3).
    psucc(3,4).
    psucc(4,5).
    psucc(5,6).
    psucc(6,7).
    psucc(7,8).
    psucc(8,9).
    psucc(9,10).

    less(A,B) :-
        psucc(A,B).
    less(A,B) :-
        psucc(A,C),
        less(C,B).

    pcompare(one_to_ten,A,A,=).
    pcompare(one_to_ten,A,B,<) :-
        less(A,B).
    pcompare(one_to_ten,A,B,>) :-
        less(B,A).

The idea here is to check if a number directly compares, and if not then run up or down the successor until a match is found. If the first number is lower in the list and the second number higher, then they are less than each other. If the first number is higher in the list then the first number is greater.

Once again, this method suffers from creating choice points and a lot of processing for even a smallish set of data because even after a match is found, then all possibilities need to be checked just in case there is a further match.

Successors are useful for other reasons however, for example a successor is used in the Music

#### Option 3: Direct mapping

The final option is to directly map the data so that each pair has a comparison which should remove the choice points and be very fast if done right. For example, if we want to check a tri-state app with states of 'on', 'off' and 'standby'. In this case on is greater than standby which is greater than off.

    pcompare(tri_state,A,B,C) :-
        tri_state(A,B,C).

    tri_state(on,B,C) :- on_compare(B,C).
    tri_state(off,B,C) :- off_compare(B,C).
    tri_state(standby,B,C) :- standby_compare(B,C).

    on_compare(on,=).
    on_compare(off,>).
    on_compare(standby,>).

    off_compare(off,=).
    off_compare(on,<).
    off_compare(standby,<).

    standby_compare(off,>).
    standby_compare(on,<).
    standby_compare(standby,=).

This code will create comparisons that don't create choice points BUT there is a lot of facts that need to be created if you have a large domain. This is method is how the `pchar` domain works to map all the known characters.

## Using builtin predicates to create Domains

A Domain does not necessarily need to be only Horn Clauses but care needs to be taken to preserve the logically pure environment. As an example a domain that will allow any ground term can be created using inbuilt predicates:

    purity:pcompare(ground, A, B, C) :-
        must_be(ground, A),
        must_be(ground, B),
        compare(C, A, B).

This is a general domain that will work for any ground term. It is useful but will produce errors if variables are used. The inbuilt ISO predicate `compare/3` can be used to create Domains providing there is a way to generate the answers.

The second example is the Domain of numbers between 1 and 1000, which can be generated using `between/3`

    purity:pcompare(int_1_to_1000, A, B, C) :-
        between(1, 1000, A),
        between(1, 1000, B),
        compare(C, A, B).

This Domain will generate results if variables are passed in, and will fail if the values are outside of the range of the Domain (1 to 1000).
