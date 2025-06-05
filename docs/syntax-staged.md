# Syntax of the Staged Language

The concrete syntax considerably overlaps that of OCaml except for some incompatible points like the following:

- Type names are capitalized (like `Int` or `Bool`).
- Arguments follow type constructors (like `List Int`).
- We cannot omit type annotations for the parameters of function abstractions, at least for the moment.
  * For example, we have to write `fun (x : Int) -> x + 1` instead of `fun x -> x + 1`.
- Elements in list literals are separated by commas, not by semicolons (like `[3, 1, 4]`).
- Type coercions are written by using `as` (like `v as Vec %n`).
- Top-level bindings are described by `val`, not by `let`.


## By example

### Let-expressions

```
let succ (x : Int) = x + 1 in
succ 42

(* ==> 43 *)
```


### Staging

The symbol `&` works as a _bracket_; it constructs a quoted expression. For example, the following stands for a code fragment `3 + 5` (not the immediate computation of the addition of `3` and `5`):

```
&(3 + 5)
```

Inside a bracket, the symbol `~` can be used for an _escape_; it splices a code fragment like the following:

```
let c = &(4 + 5) in
&(3 + ~c)

(* ==> &(3 + (4 + 5)) *)
```

Brackets `&` and escapes `~` form the notion of _stages_; they assign a stage (0 or 1) to every subexpression. A subexpression `e` prefixed by `&` is at stage 1, and the subexpression `&e` itself is at stage 0.


## Precise definitions

### Term expressions

```
<expr> ::=
  | <c>                                                 (constants)
  | <longx>                                             (long identifiers)
  | '(' <expr> ')'                                      (parenthesized expressions)
  | '(' <expr> ',' <expr> ')'                           (construction of tuples)
  | '[' ']'                                             (the empty list)
  | '[' <expr> [',' <expr>]* ']'                        (non-empty lists)

  | '&' <expr>                                          (brackets, i.e., quotes)
  | '~' <expr>                                          (escapes, i.e., unquotes)

  | <expr> <expr>                                       (applications)
  | <expr> '{' <expr> '}'                               (explicit applications of an implicit argument)
  | <expr> '_'                                          (explicit omission of an implicit argument)

  | <expr> 'as' <Ty>                                    (coercions)

  | <expr> <binOp> <expr>                               (binary operations)

  | 'fun' <param> '->' <expr>                           (abstractions)
  | 'rec' <ordParam> '->' 'fun' <ordParam> '->' <expr>  (fixpoints, i.e., recursive abstractions)
  | 'if' <expr> 'then' <expr> 'else' <expr>             (conditionals)

  | 'let' <letBinder> 'in' <expr>                       (let-expressions)
  | 'let' 'open' <X> 'in' <expr>                        (module open)
  | <expr> ';' <expr>                                   (sequentials)
```

- Lowercase-starting identifiers `<x>`:
  * A non-empty finite sequence of alphanumeric letters or `_` that starts with a lowercased letter (e.g., `foo`); or
  * A parenthesized binary operator `'(' <binOp> ')'` (e.g., `(+)`).
- Capitalized identifiers `<X>`:
  * A non-empty finite sequence of alphanumeric letters or `_` that starts with a capital letter (e.g., `Foo`).
- Long lowercase-starting identifiers `<longx>`:
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


### Constants

```
<c> ::=
  | '(' ')'           (the unit value)
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

<row> ::= <n> [',' <n>]*
```


### Type expressions

```
<Ty> ::=
  | <X>                                      (base types)
  | <tyvar>                                  (type variables)
  | '(' <x> ':' <X> [<Arg>]* '|' <expr> ')'  (refinement types)
  | '(' <Ty> ')'                             (parenthesized expressions)

  | '&' <Ty>                                 (code types)

  | <X> <Ty>                                 (applications of type constructors to type arguments)
  | <X> <expr>                               (applications of type constructors to constant term arguments)
  | <X> '%' <expr>                           (applications of type constructors to persistent term arguments)

  | <Ty> '*' <Ty>                            (product types)

  | <Ty> '->' <Ty>                           (non-dependent function types)
  | '(' <x> ':' <Ty> ')' '->' <Ty>           (dependent function types)
  | '{' <x> ':' <Ty> '}' '->' <Ty>           (dependent function types with implicit parameters)
  | 'forall' <tyvar> '->' <Ty>               (polymorphic types)
```

The following base types are pre-defined:

* Basics:
  - `Unit`
  - `Bool`
  - `Int`
  - `Nat`
    * A shorthand for the refinement type `( n : Int | 0 <= n )`.
  - `Float`
  - `String`
  - `List`
    * Takes a type argument as usual, i.e., used in the form `List <Ty>`.
  - `Tensor`
    * The type for tensors.
    * Takes a list of constant natural numbers at stage 0 (like `Tensor [2, 3]`); no computation is allowed in the arguments.
    * Takes a persistent term expression of type `List Nat` at stage 1 (like `Tensor %[2 * n, m + 1]`).
  - `Vec`
    * The type for vectors (i.e., one-dimentional tensors).
    * `Vec <n>` at stage 0 is a shorthand for `Tensor [<n>]`.
    * `Vec %<expr>` at stage 1 is a shorthand for `Tensor %[<expr>]`.
  - `Mat`
    * The type for matrices (i.e., two-dimensional tensors).
    * `Mat <n_1> <n_2>` at stage 0 is a shorthand for `Tensor [<n_1>, <n_2>]`.
    * `Vec %<expr_1> %<expr_2>` at stage 1 is a shorthand for `Tensor %[<expr_1>, <expr_2>]`.
* Types for handling PyTorch objects (inspired by OCamlTorch):
  - `Device`
  - `Activation`
  - `VarStore`
  - `Optimizer`


### Bindings

```
<bind> ::=
  | 'val' <valBind>
  | 'module' <X> '=' 'struct' [<bind>]* 'end'
```
