Up: [Purity](intro.md)

## Domains

A domain contains facts and rules about a particular type of knowledge. From Purity's point of view, a domain is a type of data, but custom domains can be created to contain any set of facts and rules. Several library calls in Purity have a domain as a parameter and this call will usually use the domain parameter to do comparisons.

The following domains come standard with the Purity library:

- `pchar` - A domain containing the following characters:

  - The character set `[a-zA-Z0-9]`
  - The digits `[0-9]`
  - The following symbols `;:",?/\<>,.!@#$%^&*()[]{}|-_+=`
  - Whitespace characters `<space>\n\r\t`

- `pstring` - Strings which are represented by a list of `pchar` characters. pstring is the equivalent of `plist(pchar)` when using comparisons, however there are a special set of APIs that start with `pstr_` that only use the `pstring` domain.

- `plist(D)` - Lists which can be of type D where D is a domain.

- `punary` - Unary numbers (or natural numbers) using church encoding
  - zero = 0
  - c(zero) = 1
  - c(c(zero)) = 2
  - etc..
