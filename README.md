# Horsea: Compile-Time Tensor Shape Checking via Staging

## How to Use

### How to build

```console
$ cabal build
```


### How to run

#### The staged language (`staged`)

```console
$ cabal run horsea -- staged -w 80 --optimize examples/mat.lwsd
```

Options:

* `-O`, `--optimize`: Inserts only non-trivial cast assertions
* `-c`, `--compile-time-only`: Stops after the compile-time evaluation
* `-w`, `--display-width`: Sets the length of the terminal width for displaying texts. Default: `80`


### The non-staged surface language (`surface`)

```console
$ cabal run horsea -- surface examples/mat.surf
```

Options:

* `-d`, `--default-to-stage-0`: Makes ambiguous binding times default to 0, which promotes inlining
* `-O`, `--optmize`: Same as `staged`
* `-c`, `--compile-time-only`: Same as `staged`
* `-w`, `--display-width`: Same as `staged`


## Memos for Development

### How to test

```console
$ cabal test
```


## How to run code formatting

```console
$ cabal run -O1 ormolu -- --mode inplace $(git ls-files '*.hs')
```


## TODOs (theory)

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


## TODOs (implementation)

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
