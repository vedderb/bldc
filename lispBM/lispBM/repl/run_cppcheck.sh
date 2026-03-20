#!/bin/bash

date=$(date +"%Y-%m-%d_%H-%M")

logfile32="cppcheck_log_32bit_${date}.log"
logfile64="cppcheck_log_64bit_${date}.log"

if [ -n "$1" ] && [ -n "$2" ]; then
    logfile32="$1"
    logfile64="$2"
fi

suppressions=("--suppress=missingIncludeSystem"
              "--suppress=unusedFunction"
              "--suppress=constParameterPointer"
              "--suppress=constParameterCallback"
              "--suppress=constVariablePointer")

NUM_JOBS=$(nproc 2>/dev/null || echo 4)

# 32bit run 
make clean
bear -- make

cppcheck --version &> $logfile32
cppcheck -j$NUM_JOBS --inline-suppr --project=compile_commands.json --enable=all --check-level=exhaustive ${suppressions[@]} 2>> $logfile32


#64bit run
make clean
bear -- make FEATURES="64"

cppcheck --version &> $logfile64
cppcheck -j$NUM_JOBS --inline-suppr --project=compile_commands.json --enable=all  --check-level=exhaustive ${suppressions[@]} 2>> $logfile64




