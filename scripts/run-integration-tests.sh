#!/bin/bash

# Must be run at the root of the repository

TESTS_STAGED_RUN=(
    examples/gen_vrepeat.lwsd
    examples/gen_vrepeat_explicit.lwsd
    examples/mat.lwsd
    examples/mat_explicit.lwsd
    examples/mat_concat_vert.lwsd
    examples/mat_concat_vert_explicit.lwsd
    examples/polymorphic.lwsd
    examples/repeat_and_add.lwsd
    examples/repeat_and_add_explicit.lwsd
    examples/simple_rec.lwsd
    examples/tensor_add.lwsd
    examples/tensor_add_mat.lwsd
    examples/vec.lwsd
    examples/vec_explicit.lwsd
    examples/vec_higher_order.lwsd
    examples/vec_higher_order_explicit.lwsd
)
TESTS_STAGED_COMPILE=(
    examples/ocaml-torch/mnist/linear.lwsd
    examples/ocaml-torch/mnist/linear_as.lwsd
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
    examples/ocaml-torch/mnist/conv.surf
    examples/ocaml-torch/mnist/linear.surf
    examples/ocaml-torch/mnist/linear_as.surf
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
for FILE in integration_tests/failure/*.lwsd; do
    echo "======== $FILE (should be rejected) ========"
    cabal run horsea -- staged --optimize --distribute-if "$FILE"
    if [ $? -eq 0 ]; then
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
for FILE in integration_tests/failure/*.surf; do
    echo "======== $FILE (should be rejected) ========"
    cabal run horsea -- surface --optimize --distribute-if "$FILE"
    if [ $? -eq 0 ]; then
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
