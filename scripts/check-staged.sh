#!/bin/bash

# Must be run at the root of the repository

INPUT="$1"
FILE="examples/ocaml-torch/$INPUT.lwsd"
echo "======== $FILE ========"
cabal run horsea -- staged --optimize --distribute-if --compile-time-only "$FILE"
