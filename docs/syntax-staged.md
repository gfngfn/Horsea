# Syntax of the Staged Language

The concrete syntax considerably overlaps that of OCaml except for some incompatible points as follows:

- Type names are capitalized (like `Int` or `Bool`).
- Arguments follow type constructors (like `List Int`).
- We cannot omit type annotations for the parameters of function abstractions, at least for the moment.
  * For example, we have to write `fun (x : Int) -> x + 1` instead of `fun x -> x + 1`.
- Elements in list literals are separated by commas, not by semicolons (like `[3, 1, 4]`).
- Type coercions are written by using `as` (like `v as Vec %n`).
- Top-level bindings are described by `val`, not by `let`.

Also, compared to OCaml, the following constructs are added:

- Staging constructs, i.e., _brackets_ `&` and _escapes_ `~`, which are used like `&(x + 1)` and `~(gen_power n)`, respectively
- Expression arguments of type constructors like `n * 2` and `m + 1` of `Tensor %[n * 2, m + 1]`
- Abstractions and applications for _implicit parameters/arguments_

## By example

### `let`/`let rec`-expressions and conditionals

`let`-expressions and conditionals (i.e., `if`-expressions) are basically the same as OCaml:

```
let succ (x : Int) = x + 1 in

succ 42  (* ==> 43 *)
```

However, for the moment, `let rec`-expressions have the following two constraints:

- Return types must be annotated.
  * This is due to the bi-directional type inference algorithm.
- Mutual recursion is not supported.

```
let rec fact (n : Int) : Int =
  if n <= 0 then
    1
  else
    n * fact (n - 1)
in

fact 5  (* ==> 120 *)
```


### Staging constructs and their typing

The symbol `&` works as a _bracket_; it constructs a quoted expression (i.e., `&e` corresponds to `〈e〉` in the standard notation of MetaML). For example, the following stands for a _code fragment_ `3 + 5` (not the immediate computation of the addition of `3` and `5`):

```
&(3 + 5)
```

Inside brackets, the symbol `~` can be used as an _escape_; it splices a code fragment like the following:

```
let c = &(4 + 5) in

&(3 + ~c)  (* ==> &(3 + (4 + 5)) *)
```

Brackets `&` and escapes `~` form the notion of _stages_; they assign a stage (0 or 1) to every subexpression. A subexpression `e` prefixed by `&` is at stage 1, and the surrounding stuff outside the bracket is at stage 0. Conversely, a subexpression `e` prefixed by `~` is at stage 0, and the surrounding stuff is at stage 1. Subexpressions at stage 0 are to be evaluated at compile-time computation, while those at stage 1 are left for runtime computation.

We also have _code types_ of the form `&τ` to assign types to code fragments. A bracketed expression `&e` is assigned type `&τ` at stage 0 if `e` is of type `τ` at stage 1. Conversely, `~e` is assigned type `τ` if `e` is of type `&τ`.

For example, suppose the following function `gen_power`, which takes a natural number `n` and returns a code fragment of the `n`-th power function:

```
let gen_power (n : Int) =
  let rec aux (n : Int) (c : &Int) : &Int =
    if n <= 0 then
      &1
    else
      &(~c * ~(aux (n - 1) c))
  in
  &(fun(x : Int) -> ~(aux n &x))
```

This function is assigned type `Int -> &(Int -> Int)`.


### Value arguments of types and dependent function types

For stage-1 vectors, we have type `Vec %e`, where `e` is a stage-0 expression (dubbed as _value arguments_) that stands for the length of the vectors assigned that type. Similarly, for stage-1 matrices, we have type `Mat %e_1 %e_2`, where `e_1` and `e_2` correspond to the number of rows and columns, respectively. Also, these two type constructors are generalized to tensors of arbitrary shapes by `Tensor`; is takes a list of natural numbers. That is, `Vec %e` and `Mat %e_1 %e_2` are syntax sugar of `Tensor %[e]` and `Tensor %[e_1, e_2]`, respectively.

Consider the type of the vector addition operation, for example: Since it is defined for two vectors of the same length, the stage-1 vector addition operation specialized for length `n` must have type `Vec %n -> Vec %n -> Vec %n` (Note: `n` is a metavariable so far). To this end, we can provide a stage-0 built-in function `gen_vadd` for generating such specialized operations with type `(n : Int) -> &(Vec %n -> Vec %n -> Vec %n)` (Note: `n` is now an object-level variable). Here, function types of the form `(x : τ_1) -> τ_2` are called _dependent function types_, meaning that the codomain type `τ_2` can depend on the value-level variable `x` standing for the parameter.

We can describe vector addition at stage 1 like `~(gen_vadd 5) [|3; 1; 4; 1; 5|] [|9; 2; 6; 5; 3|]`.


### Implicit parameters/arguments

The actual type of `gen_vadd` provided by the stub file is `{n : Nat} -> &(Vec %n -> Vec %n -> Vec %n)`. The type `Nat` for natural numbers will be introduced in the next section, and we will here focus on function types of the form `{x : τ_1} -> τ_2`; they are for functions whose parameter can be _implicit_ (i.e., omissible because the typechecker can infer an expression for it based on the contexts). Owing to this, we can just write `~gen_vadd [|3; 1; 4; 1; 5|] [|9; 2; 6; 5; 3|]`, and the typechecker will infer that the parameter `n` should be instantiated with the expression `5` here. We can nonetheless explicitly pass an argument for the omissible parameter like `gen_vadd {5}`. Or, we can also write `gen_vadd _` for the _explicit omission_ of an implicit argument.


### Refinement types for stage-0 computation

We provide _refinement types_ `(x : τ | e)` for stage-0 types; unlike popular notations that use brackets, we currently use parentheses. Intuitively, this is a “subset” of the type `τ` consisting of all the `τ`-typed values that satisfy the predicate `e`; `e` is an arbitrary stage-0 expression of type `τ -> Bool`. The type `Nat` for natural numbers is syntax sugar for `(n : Int | 0 <= n)`.


### Basic syntax of top-level bindings

The _stub file_ (i.e., the source file that defines built-in functions) consists of a sequence of _top-level bindings_ (or simply _bindings_).

There are three kinds of bindings: _stage-1 bindings_, _stage-0 bindings_, and _persistent bindings_. First, stage-1 bindings are quite ordinary; they are just function defintions used at runtime:

```
val succ (n : Int) = n + 1
```

The parameters and the right-hand side of stage-1 bindings are treated as stage-1 stuff, and the bound name will be available as a stage-1 value. Stage-0 bindings are for compile-time computation:

```
val ~succ (n : Int) = n + 1
```

The parameters and the right-hand side live in stage 0, and the bound name will be available as a stage-0 value. Using these bindings we can do the following, for example:

```
val ~c_offset = &(42 + 57)
val shift (n : Int) = n + ~c_offset
```

At runtime, `shift` is bound to a function `fun(n : Int) -> n + 99`.

Values that do not depend on neither the staging constructs nor stage-specific values can be _persistent_ (i.e., available at both stages); we can use persistent bindings for this:

```
val %succ (n : Int) = n + 1
```


### Top-level delcarations of built-in functions

The type of a built-in function can be declared through bindings using `external`:

```
val %( + ) : Int -> Int -> Int
  external (builtin = "int_add", surface = "+")

val ~gen_vadd : {a : Nat} -> &(Vec %a -> Vec %a -> Vec %a)
  external (builtin = "gen_vadd", surface = "vadd")
```

The comma-separated key–val entries specify some metadata. The field `builtin` associates a internal name (e.g., `int_add`) with the identifier (e.g., `( + )`), and `surface` binds a name for the non-staged surface language.


### Modules

A number of bindings can be bundled as a _module_ by the `module` syntax:

```
module Foo = struct
  val ~offset = &(42 + 57)
  val shift (n : Int) = n + ~offset
end
```

In this case, members can be used as `Foo.c_offset` at stage 0 and `Foo.shift` at stage 1, respectively.


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
  | <expr> '|>' <expr>                                  (flipped applications)

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

```
<binOp> ::=
  | <multBinOp>
  | <addBinOp>
  | <compBinOp>
```

- Multiplicative binary operators `<multBinOp>`:
  * A non-empty finite sequence of _operator symbols_ that starts with `*` or `/`, where the operator symbols are `+`, `-`, `*`, `/`, `=`, `<`, and `>`.
- Additive binary operators `<addBinOp>`:
  * A non-empty finite sequence of operator symbols that starts with `+` or `-`.
- Comparative binary operators `<compBinOp>`:
  * A non-empty finite sequence of operator symbols that starts with `=`, `<`, or `>`.


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
  | 'val' <valBinder> <bindVal>
  | 'module' <X> '=' 'struct' [<bind>]* 'end'

<valBinder> ::=
  | '~' <x>  (binders of stage-0 values)
  | <x>      (binders of stage-1 values)
  | '%' <x>  (binders of persistent values)

<bindVal> ::=
  | '=' <expr>                             (ordinary definitions)
  | ':' <Ty> 'external' '(' <extSpec> ')'  (bindings for built-in values)

<extSpec> ::=
  | (empty)
  | <x> '=' <str> [',' <x> '=' <str>]*
```
