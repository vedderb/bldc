#!/bin/bash

src=$1

valgrind --toggle-collect=lbm_run_eval --tool=callgrind --callgrind-out-file=cg.out ./repl --terminate -s $src

gprof2dot  -f callgrind cg.out -o cg.dot

dot -Tpdf cg.dot -o cg.pdf

rm cg.out
rm cg.dot
