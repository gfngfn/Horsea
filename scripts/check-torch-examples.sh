#!/bin/bash

# Must be run at the root of the repository

STAGED_INPUTS=(
    "mnist/linear"
    "mnist/linear_as"
)
SURFACE_INPUTS=(
    "mnist/conv"
    "mnist/linear"
    "mnist/linear_as"
)

ERRORS=()

for INPUT in "${STAGED_INPUTS[@]}"; do
    FILE="examples/ocaml-torch/$INPUT.lwsd"
    echo "======== $FILE ========"
    cabal run horsea -- staged --optimize --distribute-if --compile-time-only "$FILE"
    if [ $? -ne 0 ]; then
        ERRORS+=("$FILE")
    fi
done

for INPUT in "${SURFACE_INPUTS[@]}"; do
    FILE="examples/ocaml-torch/$INPUT.surf"
    echo "======== $FILE ========"
    cabal run horsea -- surface --optimize --distribute-if --compile-time-only "$FILE"
    if [ $? -ne 0 ]; then
        ERRORS+=("$FILE")
    fi
done

RET=0
for ERROR in "${ERRORS[@]}"; do
    RET=1
    echo "! FAILED: $ERROR"
done
if [ $RET -eq 0 ]; then
    echo "All the runs were successful."
fi
exit $RET
