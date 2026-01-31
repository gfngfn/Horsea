#!/bin/bash

# Must be run at the root of the repository

TESTS_STAGED_RUN=(
    examples/gen_vrepeat.lba
    examples/gen_vrepeat_explicit.lba
    examples/mat.lba
    examples/mat_explicit.lba
    examples/mat_concat_vert.lba
    examples/mat_concat_vert_explicit.lba
    examples/polymorphic.lba
    examples/repeat_and_add.lba
    examples/repeat_and_add_explicit.lba
    examples/simple_rec.lba
    examples/tensor_add.lba
    examples/tensor_add_mat.lba
    examples/vec.lba
    examples/vec_explicit.lba
    examples/vec_higher_order.lba
    examples/vec_higher_order_explicit.lba
)
TESTS_STAGED_COMPILE=(
    examples/ocaml-torch/jit/load_and_run.lba
    examples/ocaml-torch/mnist/conv.lba
    examples/ocaml-torch/mnist/linear.lba
    examples/ocaml-torch/mnist/linear_as.lba
    examples/ocaml-torch/pretrained/finetuning.lba
    examples/ocaml-torch/pretrained/predict.lba
    examples/ocaml-torch/char_rnn/char_rnn.lba
    examples/ocaml-torch/gan/mnist_cgan.lba
    examples/ocaml-torch/gan/mnist_dcgan.lba
    examples/ocaml-torch/gan/mnist_gan.lba
)
TESTS_STAGED_FAILURE=(
    examples/failure/error-bracket.lba
    examples/failure/error-invalid-type-1.lba
    examples/failure/error-not-code-typed.lba
    examples/failure/error-scope-2.lba
    examples/failure/error-dep-fun-at-stage-1.lba
    examples/failure/error-list.lba
    examples/failure/error-not-int-lit-arg.lba
    examples/failure/error-scope.lba
    examples/failure/error-escape.lba
    examples/failure/error-nat.lba
    examples/failure/error-not-int-typed-arg.lba
    examples/failure/error-invalid-type-0.lba
    examples/failure/error-normal-arg-at-stage-1.lba
    examples/failure/error-persistent-arg-at-stage-0.lba
    examples/failure/error-var-not-stage-0.lba
)
TESTS_SURFACE_RUN=(
    examples/gen_vrepeat.surf
    examples/gen_vrepeat_explicit.surf
    examples/mat.surf
    examples/mat_explicit.surf
    examples/mat_concat_vert.surf
    examples/mat_concat_vert_explicit.surf
    examples/repeat_and_add_explicit.surf
    examples/repeat_and_add.surf
    examples/simple_rec.surf
    examples/tensor_add.surf
    examples/tensor_add_mat.surf
)
TESTS_SURFACE_COMPILE=(
    examples/ocaml-torch/jit/load_and_run.surf
    examples/ocaml-torch/mnist/conv.surf
    examples/ocaml-torch/mnist/linear.surf
    examples/ocaml-torch/mnist/linear_as.surf
    examples/ocaml-torch/pretrained/finetuning.surf
    examples/ocaml-torch/pretrained/predict.surf
    examples/ocaml-torch/char_rnn/char_rnn.surf
    examples/ocaml-torch/gan/mnist_cgan.surf
    examples/ocaml-torch/gan/mnist_dcgan.surf
    examples/ocaml-torch/gan/mnist_gan.surf
)
TESTS_SURFACE_FAILURE=(
    examples/failure/error-stage.surf
)

ERRORS=()

for FILE in "${TESTS_STAGED_RUN[@]}"; do
    echo "======== $FILE (should pass) ========"
    cabal run horsea -- staged --optimize --distribute-if "$FILE"
    if [ $? -ne 0 ]; then
        ERRORS+=("$FILE (should pass)")
    fi
done
for FILE in "${TESTS_STAGED_COMPILE[@]}"; do
    echo "======== $FILE (should pass, compile-time only) ========"
    cabal run horsea -- staged --optimize --distribute-if --compile-time-only "$FILE"
    if [ $? -ne 0 ]; then
        ERRORS+=("$FILE (should pass, compile-time only)")
    fi
done
for FILE in "${TESTS_STAGED_FAILURE[@]}"; do
    echo "======== $FILE (should be rejected) ========"
    cabal run horsea -- staged --optimize --distribute-if "$FILE"
    if [ $? -le 1 ]; then
        ERRORS+=("$FILE (should be rejected)")
    fi
done

for FILE in "${TEST_SURFACE_RUN[@]}"; do
    echo "======== $FILE (should pass) ========"
    cabal run horsea -- surface --optimize --distribute-if "$FILE"
    if [ $? -ne 0 ]; then
        ERRORS+=("$FILE (should pass)")
    fi
done
for FILE in "${TESTS_SURFACE_COMPILE[@]}"; do
    echo "======== $FILE (should pass, compile-time only) ========"
    cabal run horsea -- surface --optimize --distribute-if --compile-time-only "$FILE"
    if [ $? -ne 0 ]; then
        ERRORS+=("$FILE (should pass, compile-time only)")
    fi
done
for FILE in "${TESTS_SURFACE_FAILURE[@]}"; do
    echo "======== $FILE (should be rejected) ========"
    cabal run horsea -- surface --optimize --distribute-if "$FILE"
    if [ $? -le 1 ]; then
        ERRORS+=("$FILE (should be rejected)")
    fi
done

RET=0
for ERROR in "${ERRORS[@]}"; do
    RET=1
    echo "! FAILED: $ERROR"
done
if [ $RET -eq 0 ]; then
    echo "All tests have passed."
fi
exit $RET
