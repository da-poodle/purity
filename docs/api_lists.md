Up: [Purity](intro.md)

## List Predicates

To use, import the following module

    :- use_module(library(purity)).

### plist/1

    plist(L).

holds if L is a valid list

### plength/2

    plength(List, Length).

Length is the number of elements in List using the punary domain.

### pnth0/3

    pnth0(Nth, Val, List).

Val is the Nth element in List starting at zero.

Nth is a punary number.

### pnth1/3

    pnth1(Nth, Val, List).

Val is the Nth element in List starting at c(zero).

Nth is a punary number.

### pmemberchk/3

    pmemberchk(Domain, Element, List).

check if Element exists in List once only.

### psublist/5

    psublist(List, Before, Length, After, SubList).

SubList is contained within List in the same sequence.

Before is the number of elements before SubList

Length is the number of elements in SubList

After is the number of elements after SubList

### ppartition/4

    ppartition(Goal, List, Included, Excluded).

Using Goal as the decider, the elements of list are either in Included or Excluded.

Included and Excluded are also lists.

Goal is a predicate that has a `pbool` as the last argument.

### pinclude/3

    pinclude(Goal, List, Included).

Included is the List with the elements that are true for Goal.

Goal is a predicate that has a `pbool` as the last argument.

### pexclude/3

    pexclude(Goal, List, Excluded).

Excluded is the List without the elements that are true for Goal.

Goal is a predicate that has a `pbool` as the last argument.

### list_in_domain/2

    list_in-domain(Domain, List).

Holds if all elements of List are in Domain.

### non_member/3

    non_member(Domain, Element, List).

Holds if Element is not an element in List.

the Element and List must be a member of Domain.

### remove_dups/2

    remove_dups(List, NoDups).

NoDups is List without any repeating items.

### list_join/3

    list_join(ListOfLists, DelimList, ResultList).

ResultList is the ListOfLists flattened with DelimList separating each list

eg: ListOfLists = [[a],[b],[c]], DelimList = [','], ResultList = [a,',',b,',',c]

### psort/3

    psort(Domain, List, Sordered)

Sorted is an ordered version of List.

all elements in List and Sorted must be in Domain.
