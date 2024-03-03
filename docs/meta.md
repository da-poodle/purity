Up: [Purity](intro.md)

## Horn Clause Only Meta predicates

Purity is designed to be used on a system that ONLY has Horn Clauses available, but all Prolog systems have the capability to run Purity code, and in fact there are several inbuilt calls which are perfectly fine to use in the standard Prolog library and are considered 'pure', but will not work in a horn clause only system.

The main difference is around meta predicates, and if predicates like `maplist/n` are used, then extra code will need to be written to get the program to work on a Horn Clause only system.

`call/n` can be easily emulated by creating a call predicate with a map to the predicate that is to be called, for example if you want to call `pstr_upper/2` for two string lists:

    call(pstr_upper, S1, S1) :- pstr_upper(S1, S2)

then the `maplist/3` call can be used from the stand-alone Purity library:

    maplist(pstr_upper, ["test", "toast"], Upper).

Other meta predicates can be implemented as well, like `=..` for example. The term to transform will be `test(A,B)`

    =..(test(A,B), [test,A,B]).

`functor/3` is possible as well with a direct mapping.

    functor(test(_,_), test, 2).

`arg/3` can be implemented using unary numbers, if you like ... using `pnth1/3`

    arg(N, test(A,B), Arg) :-
        nth1(N, [A,B], Arg).

Meta predicates are useful for writing generic code that is flexible and small, and as can be seen, this is still possible when using only Horn Clauses with a few adjustments.

Having said that, some meta predicates are not possible, but that is because they are not logical, like `copy_term/2` for example.
