Up: [Purity](intro.md)

## Set Predicates

To use, import the following module

    :- use_module(library(pset)).

### pset_empty/2

    pset_empty(Domain, Set).

Set is the empty set for Domain.

### list_set/3

    list_set(Domain, List, Set).

Set is a set of unique and sorted elements of List.
List elements must all be in Domain.

### set_list/2

    set_list(Set, List).

List is the data for Set as a list.

### subset/3

    subset(SubSet, Set, Truth).

Truth is true if all elements in SubSet are also in Set.
Truth is false if any elements in SubSet are not in Set.

### punion/3

    punion(Set1, Set2, ResultSet).

ResultSet is the union of Set1 and Set2.

### pintersection/3

    pintersection(Set1, Set2, ResultSet).

ResultSet is the intersection of Set1 and Set2.

This means that all elements

### pdifference/3

    pdifference(Set1, Set2, ResultSet).

ResultSet containst elements in Set1 that don't exist in Set2.
