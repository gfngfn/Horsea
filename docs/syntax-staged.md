# Syntax of the Staged Language

## Term expressions

```
<expr> ::=
  | <c>                                                 (constants)
  | <longx>                                             (long identifiers)
  | '(' <expr> ')'                                      (parenthesized expressions)
  | '(' <expr> ',' <expr> ')'                           (construction of tuples)

  | '&' <expr>                                          (brackets, i.e., quotes)
  | '~' <expr>                                          (escapes, i.e., unquotes)

  | <expr> <expr>                                       (applications)
  | <expr> '{' <expr> '}'                               (explicit applications of an implicit argument)
  | <expr> '_'                                          (explicit omission of an implicit argument)

  | <expr> 'as' <Ty>                                    (coercions)

  | <expr> <binop> <expr>                               (binary operations)

  | 'fun' <param> '->' <expr>                           (abstractions)
  | 'rec' <ordParam> '->' 'fun' <ordParam> '->' <expr>  (fixpoints, i.e., recursive abstractions)
  | 'if' <expr> 'then' <expr> 'else' <expr>             (conditionals)

  | 'let' <letBinder> 'in' <expr>                       (let-expressions)
  | 'let' 'open' <X> 'in' <expr>                        (module open)
  | <expr> ';' <expr>                                   (sequentials)
```

- Lowercase-starting identifiers `<x>`:
  * A non-empty finite sequence of alphanumeric letters or `_` that starts with a lowercased letter (e.g., `foo`).
- Capitalized identifiers `<X>`:
  * A non-empty finite sequence of alphanumeric letters or `_` that starts with a capital letter (e.g., `Foo`).
- Long lower identifiers `<longx>`:
  * A lowercase-starting identifier possibly prefixed with a finite number of module names (i.e., capitalized identifiers) and dots, where no spaces are allowed around dots (e.g., `Foo.Bar.qux`)

```
<letBinder> ::=
  | <x> [<param>]* '=' <expr>        (non-recursive definitions)
  | 'rec' <x> [<param>]* '=' <expr>  (recursive definitions)
  | '(' <x> ',' <x> ')' '=' <expr>   (decomposition of tuples)

<param> ::= <ordParam> | <impParam>
<ordParam> ::= '(' <x> ':' <Ty> ')'  (ordinary parameters)
<impParam> ::= '{' <x> ':' <Ty> '}'  (implicit parameters)
```


## Constants

```
<c> ::=
  | '(' ')'           (unit value)
  | 'true' | 'false'  (Booleans)
  | <n>               (non-negative integers)
  | <fl>              (non-negative floating-point numbers)
  | <str>             (strings)
  | <vec>             (column vector constants)
  | <mat>             (matrix constants)
```

- Non-negative integers `<n>`:
  * `0`, or a non-empty finite sequence of digits that does not start with `0` (e.g. `42`).
- Floating-point numbers `<fl>`:
  * A sequence consitituted by a concatenation of (1) integer literals, (2) the dot `.`, and (3) a non-empty finite sequence of digits (e.g. `42.5`).
- Strings `<str>`:
  * Any finite sequence of characters enclosed by two `"`s, where `"` and `\` are represented by `\"` and `\\`, respectively.

A vertical vector constant `<vec>` is a finite sequence of integer literals separated by `;` enclosed by `[|` and `|]` (e.g. `[| 3; 1; 4 |]`):

```
<vec> ::=
  | '[|' '|]'
  | '[|' <n> [';' <n>]* '|]'
```

A Matrix contant `<mat>` is a finite sequence of rows separated by `;` enclosed by `[#` and `#]`, where each row is a finite sequence of integer literals separated by `,` (e.g. `[# 4, 2; 5, 5 #]`):
```
<mat> ::=
  | '[#' '#]'
  | '[#' <row> [';' <row>]* '#]'

<row> ::=
  | <n> [',' <n>]*
```
