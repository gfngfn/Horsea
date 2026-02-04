# Compile-Time Tensor Shape Checking via Staged Shape-Dependent Types

## How to Use

### How to build

```console
$ cabal build
```


### How to run

#### The staged language λ⟨⟩⦇⦈ (`staged`)

```console
$ cabal run horsea -- staged examples/mat.lba
```

Options:

* `-c`, `--compile-time-only`: Stops after the compile-time evaluation.
* `-w`, `--display-width`: Sets the length of the terminal width for displaying texts. Default: `120`
* `-s`, `--stub`: Specify the location of the stub file. Default: `stub.lbam`
* `--show-parsed`: Displays the parsed program.
* `--show-elaborated`: Displays the elaborated program.
* `--show-inferred`: Displays the inferred argument for each implicit parameter.
* `--stats-only`: Prints only statistics.
* `--insert-trivial`: Inserts trivial cast assertions as well as non-trivial ones.
* `--suppress-if-distribution`: Does not perform fine-grained merging of types when checking `if`-expressions.


#### The non-staged surface language Horsea (`surface`)

```console
$ cabal run horsea -- surface examples/mat.hrs
```

Options:

* `-d`, `--default-to-stage-0`: Makes ambiguous binding times default to 0, which promotes inlining.
* `--show-binding-time`: Displays the result of binding-time analysis.
* The other options are the same as `staged`.


## Memos for Development

### How to run tests

```console
$ cabal test
```

```console
$ ./scripts/run-integration-tests.sh
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
- [ ] Binding of type names like `type Nat = { n : Int | n >= 0 }`
- [ ] `let`-expressions for persistent values
- [ ] Some evaluation during type-checking
