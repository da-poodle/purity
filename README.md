# Purity

A Horn Clause only library for Prolog.

For more details documentation see the [docs folder](docs/intro.md)

## Overview

Prolog was designed with a minimal set of operations and structures in mind, the following is all you need to make up any Turing Complete program:

- Atoms:
  - `a`
  - `12`
  - `'Hello, World'`
- Compound terms
  - `a(b)`
  - `person('Fred', age(23), gender(male))`
  - `[list, are, compound|Terms]`
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
  - `term1(x). term1(y).`
  - Note only disjunctions using the same term are allowed, and `A ; B` is not.
- `true`

The modern Prolog language has added a number of new operations on top of these, but they all involve using the cut (!) operator, which is impure.

Purity aims to implement ways to do the same things as Prolog programs that use a cut operator, but by only using the basic Horn clause operations.
