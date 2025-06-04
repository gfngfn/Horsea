# Lightweight Dependent Types via Staging

## Abstract

Dependent types are nice to have, particularly for vector/matrix computations; without dependent types such as `Vec n` or `Mat m n`, the inconsistency about the size of vectors/matrices will be detected only as a runtime error. At the same time, however, type-checking using dependent types is usually somewhat tough for users since it will require proofs for judging type equality. Either manual or automated proof is cumbersome for the real-world software development; the manual one easily imposes too many trivial proofs on users and often prevents refactoring, and the automated one is often too time-consuming for CI/CD due to the query for some back-end SMT solvers.

This work attempts to remedy the issue above by using *staging*. Based on the observation that the size of vectors/matrices can be statically determined in a broad sense in many typical cases, we judge such consistency by assertions evaluated as stage-0 (i.e. compile-time) computations. Under this kind of formalization, we can verify the consistency *almost statically* in the sense that inconsistencies as to the size of vectors/matrices will be immediately detected as assertion failures during stage-0 evaluation. In other words, we can do *macro expansion as validation*.


## TODO (theory)

- [x] Assertions corresponding to the equality check of higher-order types
  * We can probably use *Gradual tensor shape checking* \[Hattori, Kobayashi, & Sato 2023\] as a reference
- [x] Error localization for assertion failures
  * This is actually easy because it basically suffices to just add a label `L` to `assert` when type-checking applications `(e_1 e_2)^L` (with a label `L` that indicates a code position) and generating their corresponding `assert`s
- [x] Prove Soundness of assertion insertion
- [x] Prove Preservation
- [x] Prove Progress
- [x] A surface language
- [x] Infer some obvious stage-0 annotations
- [ ] Prove the validity of the surface language in some sense


## TODO (implementation)

- [x] Assertions corresponding to the equality check of higher-order types
- [x] Command-line options
- [x] Handle code positions
- [x] Add `Mat m n` and operations on it
- [x] Add realistic examples (specifically, ones using PyTorch or ocaml-torch)
- [x] Full-fledged refinement types `( x : τ | φ )`
  * This is beneficial for handling *broadcasting* of tensors in PyTorch, for example
- [x] Represent broadcasting of tensor shapes by refinement types
- [x] `let`- and `let rec`-expressions as syntax sugar
- [x] Type-checking in a bi-directional manner as to return types
- [x] Generalize the `external` syntax for various backends
- [x] Handle variable names correctly
- [x] Polymorphic types
- [ ] Transpilation to Python or OCaml
- [ ] ADTs and pattern matching
- [ ] The `run`-primitive
- [ ] Binding of type names like `type Nat = ( n : Int | n >= 0 )`
- [ ] `let`-expressions for persistent values
- [ ] Some evaluation during type-checking


## Memo

### How to Build

```console
$ cabal build
```


### How to run

#### The staged language (`lwsd`)

```console
$ cabal run lw-staged-deptype -- lwsd -w 80 --optimize examples/mat.lwsd
```

Options:

* `-O`, `--optimize`: Inserts only non-trivial cast assertions
* `-c`, `--compile-time-only`: Stops after the compile-time evaluation
* `-w`, `--display-width`: Sets the length of the terminal width for displaying texts. Default: `80`


### The non-staged surface language (`surface`)

```console
$ cabal run lw-staged-deptype -- surface examples/mat.surf
```

Options:

* `-d`, `--default-to-stage-0`: Makes ambiguous binding times default to 0, which promotes inlining
* `-O`, `--optmize`: Same as `lwsd`
* `-c`, `--compile-time-only`: Same as `lwsd`
* `-w`, `--display-width`: Same as `lwsd`


### How to test

```console
$ cabal test
```


## How to run code formatting

```console
$ cabal run -O1 ormolu -- --mode inplace $(git ls-files '*.hs')
```
