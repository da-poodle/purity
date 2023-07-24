# Purity

A Horn Clause only library for Prolog.

For more detailed documentation and API reference see: https://buslogic.pro/content/purity

## Overview

Prolog was designed with a minimal set of operations and structures in mind, the following is all you need to make up any Turing Complete program:

- Atoms:
  - `a`
  - `12`
  - `'Hello, World'`
- Compound terms
  - `a(b)`
  - `person('Fred', age(23), gender(male))`
- Variables:
  - `A`
  - `Varname`
- Facts:
  - `mortal(socrates).`
- Rules:
  - `alive(X) :- not_dead(X).`
- Conjunctions:
  - A, B.
- Disjunctions:
  - A ; B.
- True
- Fail

The modern Prolog language has added a number of new operations on top of these, but they all involve using the cut (!) operator, which is impure.

Purity aims to implement ways to do the same things as Prolog programs that use a cut operator, but by only using the basic Horn clause opererations.
