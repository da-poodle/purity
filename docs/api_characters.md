Up: [Purity](intro.md)

# pchar Domain

The `pchar` domain considers characters.

## Character Predicates

### pchar_upper/2

    pchar_upper(Char, UpperChar).

UpperChar is the uppercase version of Char

### pchar_lower/2

    pchar_lower(Char, LowerChar).

LowerChar is the lowercase version of Char

### pchar_type/2

    pchar_type(Char, Type).

Type is the major type Char

Type is one of alpha, digit, symbol, or whitespace

### pchar_code/2

    pchar_lower(Char, Code).

Code is the integer value of Char (ascii code)
