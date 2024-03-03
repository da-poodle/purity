Up: [Purity](intro.md)

## Purity Quick Start

There are two versions of Purity, a standalone version that contains no module information and a package that can be installed using SWI-Prolog.

### SWI-Prolog Package

1. Install the purity pack, following the instructions.

   ?- pack_install(purity).

1. Set the prolog flag to allow double quotes to become chars.

   ?- set_prolog_flag(double_quotes, chars).

1. Include the purity libraries in the code.

   ?- use_module(library(pstring)).
   ?- use_module(library(plist)).

1. Write a query

   ?- assert((
   purity_test(S, R) :-
   pstr_split(S, ' ', Split),
   pexclude((eq(pstring, "is"), Split, Small),
   pstr_join(Small, '-', R)
   )).
   true.

   ?- purity_test("This is a test", R), maplist(write, R), nl.
   This-a-test
   R = ['T', h, i, s, -, a, -, t, e|...].

Ready for action.
