Up: [Purity](intro.md)

## Comparision Predicates

To use, import the following module

    :- use_module(library(purity)).

### pcompare/4

    pcompare(Domain, TermA, TermB, Comparator) -

Comparator is one of <, > or =

Comparator is the comparison between TermA and TermB

both TermA and TermB must be in Domain

### pdif/3

    pdif(Domain, TermA, TermB).

TermA is not TermB

TermA and TermB must be in Domain

### pdif/4

    pdif(Domain, TermA, TermB, Result).

Result is true if TermA is not TermB for domain, otherwise false

### pif/3

    pif(Goal, TrueGoal, FalseGoal).

Goal is a callable with the last argument as a pbool.

if Goal results in true then TrueGoal is called.

if Goal results in false then FalseGoal is called.

### eq/3

    eq(Domain, TermA, TermB).

TermA and TermB are equal for Domain.

### eq/4

    eq(Domain, TermA, TermB, Result).

Result is true if TermA and TermB are equal for Domain.

Result is false if TermA and TermB are equal for Domain.

### lt/3

    lt(Domain, TermA, TermB).

TermA is less than TermB for Domain.

### lt/4

    lt(Domain, TermA, TermB, Result).

Result is true if TermA is less than TermB for Domain.

Result is false if TermA is not less than TermB for Domain.

### lte/3

    lte(Domain, TermA, TermB).

TermA is less than or equal to TermB for Domain.

### lte/4

    lte(Domain, TermA, TermB, Result).

Result is true if TermA is less than or equal to TermB for Domain.

Result is false if TermA is not less than or equal to TermB for Domain.

### gt/3

    gt(Domain, TermA, TermB).

TermA is greater than TermB for Domain

### gt/4

    gt(Domain, TermA, TermB, Result).

Result is true if TermA is greater than TermB for Domain.

Result is false if TermA is greater than TermB for Domain.

### gte/3

    gte(Domain, TermA, TermB).

TermA is greater than or equal to TermB for domain.

### gte/4

    gte(Domain, TermA, TermB, Result).

Result is true if TermA is greater than or equal to TermB for Domain.

Result is false if TermA is greater than or equal to TermB for Domain.
