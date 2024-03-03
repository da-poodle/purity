Up: [Purity](intro.md)

## Unary Number Predicates

To use, import the following module

    :- use_module(library(punary)).

### unary/1

    unary(Unary).

Unary is a valid unary number

### add/3

    add(UnaryA, UnaryB, Sum).

Sum is the value of UnaryA + UnaryB

### mul/3

    mul(UnaryA, UnaryB, Product)

Product is the result of UnaryA \* UnaryB

### div/4

    div(Divide, By, Quotient, Remainder)

Quotient is Divide / By and Remainder is the remainder part.

### pow/3

    pow(Unary, Factor, Power)

Power is Unary^Factor

### string_unary/2

    string_unary(String, Unary).

Unary is the punary version of String

String is a list of characters as per the `pstring` domain

### unary_string/2

    unary_string(Unary, String).

String is the `pstring` version of Unary
