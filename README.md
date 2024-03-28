# Purity

Purity is a library for Prolog that uses the most basic set of operations possible, which allowing for a rich set of functionality.

Purity is designed to be used as a testing tool for new interpretters and to experiment with 'pure prolog' features.

For more details documentation see the [docs folder](docs/intro.md)

## Overview

The following Prolog features are allowed in Purity:

- Terms:

  - Atoms
  - Compound terms
  - Lists
  - Variables

- Predicates

  - Facts
  - Rules

- Operators
  - Conjunctions
  - Disjunctions (through multiple clauses only).
  - true

The modern Prolog language has added a number of new operations on top of these, but they all involve using the cut (!) operator, which is impure.

Purity aims to implement ways to do the same things as Prolog programs that use a cut operator, but by only using the basic Horn clause operations.

## Using the library

### Setup

To run a program using Purity a normal Prolog interpretter can be used, but a few settings might need to be tweeked. The first is that Purity expects strings as lists of characters, so to make this easier to manager, the following Prolog flag should be set:

    :- set_prolog_flag(double_quotes, chars).

Next, because Purity is designed for the simplest compilers, importing library files must be done manually using either `consult/1`, or `ensure_loaded/1` inbuilt predicates. This allows for more simple mechanisms to be used as well.

For example, to load the basic library features for strings the following will work:

    :- ensure_loaded(purity).
    :- ensure_loaded(plist).
    :- ensure_loaded(pstring).

### Testing on complex Prolog systems

Because modern Prolog engines have a large set of libraries and features, many of which do not use the kind of `Pure Prolog` that the Purity library does, then it is important to be able to test the library without any advanced features on a modern system, without running and 'impure' functions from the modern system.

Do achieve this, a meta interpretter can be used to wrap queries that are meant to be 'pure'.

```
% Meta interpretter for 'pure prolog'
mi(Goal) :-
        \+ unclausable(Goal),
        clause(Goal, Body),
        mi(Body).
mi(true).
mi(','(A,B)) :-
        mi(A),
        mi(B).

unclausable((_,_)).
unclausable(true).
```

Then when running a goal, let's say a goal called `find_peeps/1` then run like the following:

```
mi(find_peeps(X)).
```

Then only the most basic operations are allowed.

## Optimisations

One issue with not using features like cuts in Prolog is that the code can easily become very inefficient, because there is no way to trim unnessecary paths. To get around this Purity exploits two compiler optimisations.

1. Tail recursion
1. First argument indexing.

Tail recursion allows for a much larger search space, which is useful in all Prolog programs, but first argument indexing is more interesting.

To ensure that choicepoints are not accumulated, ALL clauses should have a unique term as the first argument within a predicate. This is not always easy to do, but the Purity library tries as hard as possible.

## Limitations

There are some limitations of horn clause only code, and I'll give a bit of a list of the things that can't be done (as of this date).

- Input
- Output
- Finding all solutions
- Advanced maths functionality

There are workarounds for these that help in most cases, depending on what program you are writing, they discussed a bit more in the docs.

## Next steps

Currently the next step for this project is to create a more effective maths library using strings to represent numbers.

Then some examples will be written.
