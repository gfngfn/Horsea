#!/bin/bash

# Must be run at the root of the repository

EXAMPLES=(
    examples/ocaml-torch/char_rnn/char_rnn.hrs
    examples/ocaml-torch/gan/mnist_cgan.hrs
    examples/ocaml-torch/gan/mnist_dcgan.hrs
    examples/ocaml-torch/gan/mnist_gan.hrs
    examples/ocaml-torch/jit/load_and_run.hrs
    examples/ocaml-torch/min-gpt/mingpt.hrs
    examples/ocaml-torch/mnist/conv.hrs
    examples/ocaml-torch/mnist/linear.hrs
    examples/ocaml-torch/pretrained/finetuning.hrs
    examples/ocaml-torch/pretrained/predict.hrs
)

for FILE in "${EXAMPLES[@]}"; do
    echo "======== $FILE ========"
    cabal run horsea -- surface "$FILE" --compile-time-only --stats-only
done
