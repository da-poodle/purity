Up: [Purity](intro.md)

# Core Purity library

The core purity library is used by almost all other files in this set of libraries. While many of the files are optional, the core library is essential.

Contained within are mostly predicates for comparison, and other common helper predicates.

## Predicates

### unify/2

This predicate exists to replace `A = B` in regular Prolog.

### pcompare/3

    pcompare(TermA, TermB, Comparator) -

Comparator is one of <, > or =

Comparator is the comparison between TermA and TermB

both TermA and TermB must be in the same Domain

### pdif/3

    pdif(TermA, TermB, Truth).

TermA is not TermB

Truth is true if TermA is not TermB within the same domain.
Truth is false if TermA is the same as TermB within the same domain.

### pdif/2

The equivalent of `pdif(A, B, true).`

### pif/3

    pif(Goal, TrueGoal, FalseGoal).

Goal is a callable with the last argument resolving to true or false.

if Goal results in true then TrueGoal is called.

if Goal results in false then FalseGoal is called.

### eq/3, gt/3, gt3/3, lt/3, lte/3

    <comparator>(TermA, TermB, Truth).

- eq : terms are equal.
- gt : TermA is greater than TermB within the same domain.
- gte : TermA is greater than or equal to TermB within the same domain.
- lt : TermA is less than TermB within the same domain.
- lte : TermA is less than or equal to TermB within the same domain.

Truth is true if the condition is true.
Truth is false if the condition is false.

This goal fails if TermA and TermB are not in the same domain, or either TermA or TermB are not in any domain.
