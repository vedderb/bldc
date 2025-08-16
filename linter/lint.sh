#!/bin/bash

if [ $# -eq 1 ]; then
    echo "Detecting lint in $1"
else
    echo "Error: Script requires exactly one argument"
    echo "Example: ./lint.sh \"test.lisp\""
    echo ""
    echo "If no problems are detected, the output is nil"
    exit 1
fi

command="(run-lint \"$1\")"

lbm --src=simple-lint.lisp -e "$command" -M 11 -H 32768 --terminate
