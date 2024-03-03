Up: [Purity](intro.md)

## Foundations of Horn Clause only code

Horn Clause only code is a bit different from other types of programs in Prolog because there is not input or output apart from parameters and results.

Horn Clause only programs are completely self enclosed and have no library calls, and access no outside resources.

For these reasons, a Horn Clause only program will usually be a part of a larger program to create a section of code that is 'logically pure'.

There are few patterns which programs will generally take.

### Pattern 1 - Query

The most well known pattern would be the query, where the program is self enclosed and the user will type out a query, which will result in one or more answers. For example if using the infamous ancestor program, then a query might be:

    ?- is_parent_of(mike, X).

This could result is a set of answers, which in this case would be the children of mike.

This type of program is not that useful in the real world, but is useful for development and general Prolog useage.

## Pattern 2 - Input, Query, Process

It is more useful to vary the input and actually do something with the answers produced by a program and in in this pattern, the program will be called from a 'less pure' program that will provide the input and do further processing like print to the screen. This allows the Horn Clause only code to be more like a function of the program rather than the program itself, while still maintaining logical purity.

eg:

    do_program :-
        read(UserInstruction),
        call_horn_clause_code(UserInstruction, Output),
        maplist(write, Output).

Strings in Purity are lists of characters, so to print them out maplist(write, ... ) can be used.

## Pattern 3 - Query with State

The last pattern (at least the last one mentioned here) is a loop of sorts, where the the Horn Clause only code produces a answer and a 'state' which can be used to generate the next answer.

For example a program that generates Conway's Game Of Life might work like the following:

    game_of_life(InitialBoard) :-
        horn_clause_only_game_of_life(InitialBoard, ResultingBoard),
        !,
        print_board(ResultingBoard),
        game_of_life(ResultingBoard).

For this pattern a cut is generally required to remove any choice points from the Horn Clause only code. This allows the tail recursion optimisation to take place which will let the program run forever without running out of memory. Of course with really clever programming it might be possible to remove all choice points as well.

## Domains

The key to creating Horn Clause only code is to restrict the world to a well defined Domain. A domain is a 'type' of information that can be reasoned over. Once a Domain is created then and comparisons can be made over that Domain, then the Purity library will help generate Horn Clause only code.

For more information on Domains see [Creating custom Domains](domains.md)

For Domains that come with the Purity library see [Domains](api_domains.md)
