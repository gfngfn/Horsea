#!/bin/bash

# Must be run at the root of the repository

ERRORS=()

for FILE in integration_tests/success/*.lwsd; do
    echo "======== $FILE (should pass) ========"
    cabal run horsea -- staged --optimize --distribute-if "$FILE"
    if [ $? -ne 0 ]; then
        ERRORS+=("$FILE (should pass)")
    fi
done
for FILE in integration_tests/success_compile/*.lwsd; do
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

for FILE in integration_tests/success/*.surf; do
    echo "======== $FILE (should pass) ========"
    cabal run horsea -- surface --optimize --distribute-if "$FILE"
    if [ $? -ne 0 ]; then
        ERRORS+=("$FILE (should pass)")
    fi
done
for FILE in integration_tests/success_compile/*.surf; do
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
