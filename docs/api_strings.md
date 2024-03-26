Up: [Purity](intro.md)

## String Predicates

String manipulation library.

### pstr_upper/2

    pstr_upper(String, Upper).

Upper is the upper case version of String

### pstr_lower/2

    pstr_lower(String, Lower).

Lower is the lower case version of String

### pstr_split/3

    pstr_split(StrToSplit, DelimiterChar, Split).

Split is StrToSplit separated by DelimiterChar into lists.

DelimiterChar is not included in Split.

### pstr_join/3

    pstr_join(ListOfStrings, Delimiter, Joined).

Joined is ListOfStrings flattened into a single list and separated by Delimiter

### pstr_contains/2

    pstr_contains(String, SubString).

Holds if SubString is a sequence within String

### pstr_contains/3

    pstr_contains(String, SubString, Contains).

Contains is true if SubString is a sequence within String, otherwise false

### pstr_prefix/2

    pstr_prefix(Prefix, String).

Holds if Prefix is the starting sequence of String

### pstr_prefix/3

    pstr_prefix(Prefix, String, IsPrefix).

IsPrefix is true if Prefix is the starting sequence of String

### pstr_trim/2

    pstr_trim(UnTrimmed, Trimmed).

Trimmed is UnTrimmed with all whitespace removed from the start and end

### pstr_replace/4

    pstr_replace(String, Find, Replace, Replaced).

Replaced is String with the first instance of Find changed to Replace
